package artifacts

import Maven.Coordinates
import cats.data.ValidatedNel
import cats.effect.*
import cats.syntax.all.*
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.{Argument, Command, Opts}
import de.lhns.fs2.compress.*
import de.vandermeer.asciitable.AsciiTable
import de.vandermeer.skb.interfaces.transformers.textformat.TextAlignment
import fs2.io.file.{Files, Path}
import io.circe.syntax.*
import org.eclipse.aether.*
import org.eclipse.aether.repository.RemoteRepository
import org.eclipse.aether.util.repository.AuthenticationBuilder

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

sealed trait VersionFormat
object VersionFormat {
  case object JSON extends VersionFormat
  case object Table extends VersionFormat
  case object Latest extends VersionFormat

  implicit val args: Argument[VersionFormat] = new Argument[VersionFormat] {
    override def read(string: String): ValidatedNel[String, VersionFormat] =
      string match {
        case "json" => VersionFormat.JSON.validNel
        case "table" => VersionFormat.Table.validNel
        case "latest" => VersionFormat.Latest.validNel
        case x => s"'$x' is not a valid format".invalidNel
      }

    override def defaultMetavar: String = "format"
  }
}

private object Options {
  val usernameOpt =
    Opts.env[String]("MAVEN_USERNAME", help = "username for maven repository").orNone
  val passwordOpt =
    Opts.env[String]("MAVEN_PASSWORD", help = "password for maven repository.").orNone

  val usernamePasswordOpt: Opts[Option[(username: String, password: String)]] =
    (usernameOpt, passwordOpt).mapN((a, b) => (a, b).tupled)

  val repositoryNameOpt =
    Opts
      .env[String]("MAVEN_REPOSITORY", help = "Lookup in which repository")
      .withDefault("https://repo1.maven.org/maven2")

  val repositoryOpt =
    (repositoryNameOpt, usernamePasswordOpt)
      .mapN(Maven.repositoryFor)

  given Argument[Maven.Module] = new Argument[Maven.Module] {
    override def read(string: String): ValidatedNel[String, Maven.Module] =
      Maven.Module.parse(string) match {
        case Some(module) => module.validNel
        case None =>
          s"Invalid module: '${string}', expected format is <groupId>:<artifactId>".invalidNel
      }

    override def defaultMetavar: String = "coordinates"
  }

  given Argument[Maven.Coordinates] = new Argument[Maven.Coordinates] {
    override def read(string: String): ValidatedNel[String, Maven.Coordinates] =
      Coordinates.parse(string) match {
        case Some(coords) => coords.validNel
        case None =>
          s"Invalid coords: '${string}', expected format is <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>".invalidNel
      }

    override def defaultMetavar: String = "coordinates"
  }

  val moduleOpt = Opts.option[Maven.Module](
    "coordinates",
    "Coordinates for maven format is <groupId>:<artifactId>")

  val coordinatesOpt = Opts.option[Maven.Coordinates](
    "coordinates",
    "Coordinates for maven format is <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>")

  val verboseOpts: Opts[Boolean] = Opts.flag("verbose", help = "Log to standard err").orFalse
  val versionsOpts: Opts[IO[Option[Maven.Versions]]] =
    (repositoryOpt, moduleOpt, verboseOpts).mapN(Maven.versions)
  val resolveOpt: Opts[IO[Option[Maven.ResolvedArtifact]]] =
    (repositoryOpt, coordinatesOpt, verboseOpts).mapN(Maven.resolve)
  val deployOpt: Opts[IO[Unit]] =
    (
      repositoryOpt,
      coordinatesOpt,
      Opts.argument[java.nio.file.Path]("file").map(Path.fromNioPath),
      verboseOpts)
      .mapN(
        Maven.deploy
      )
}

object Main
    extends CommandIOApp(
      "micro-maven-resolver",
      "Maven Resolver and publisher",
      version = cli.build.BuildInfo.projectVersion.getOrElse("main")) {

  def runVersions(
      versions: IO[Option[Maven.Versions]],
      format: VersionFormat,
      maxVersions: Int): IO[Unit] =
    versions
      .flatMap(x => IO.fromOption(x)(new RuntimeException("Unable to resolve versions")))
      .flatMap { our =>
        format match {
          case VersionFormat.JSON =>
            IO.println(
              our.copy(versions = our.versions.take(maxVersions)).asJson.spaces2
            )

          case VersionFormat.Table =>
            IO.println {
              val table = {
                val t = new AsciiTable()

                t.addRule()
                t.addRow("coordinates", "latest", "candidates")
                t.addRule()
                t.addRow(
                  our.module.rendered,
                  our.latest.getOrElse("none"), {
                    val (toRender, extra) = our.versions.splitAt(5)
                    val rendered = toRender.mkString(",")
                    rendered + (if (extra.nonEmpty) " [...]" else "")
                  }
                )
                t.addRule()
                t.setTextAlignment(TextAlignment.CENTER)
                t
              }

              table.render()
            }
          case VersionFormat.Latest =>
            IO.fromOption(our.latest)(new RuntimeException("No latest version defined"))
              .flatMap(v => IO.println(v))
        }
      }

  def unArchive(archive: Path, writeTo: Path)(implicit files: Files[IO]): IO[Unit] =
    for {
      _ <- IO.consoleForIO.errorln(s"Extracting to ${writeTo}")
      _ <- files.createDirectories(writeTo)
      _ <- files
        .readAll(archive)
        .through(ZipUnarchiver.make[IO]().unarchive)
        .flatMap { case (entry, data) =>
          if (entry.isDirectory) {
            fs2.Stream.exec(files.createDirectories(writeTo.resolve(entry.name))) ++ data.drain
          } else {
            data.through(files.writeAll(writeTo.resolve(entry.name)))
          }
        }
        .compile
        .drain
    } yield ()

  def runDownload(action: IO[Option[Maven.ResolvedArtifact]], extractTo: Option[Path]): IO[Unit] =
    action
      .flatMap { maybeResolved =>
        for {
          resolved <- IO.fromOption(maybeResolved)(
            new RuntimeException("Unable to download dependency"))
          _ <- IO.println(resolved.path)
          _ <- extractTo.traverse_(unArchive(resolved.path, _))
        } yield ()
      }

  private def runIO(op: IO[Unit]) =
    op.as(ExitCode.Success).recoverWith { case NonFatal(e) =>
      IO.consoleForIO.errorln(s"Error occured: ${e.getMessage}").as(ExitCode(1))
    }

  override def main: Opts[IO[ExitCode]] =
    Opts.subcommands(
      Command("versions", "Versions")(
        (
          Options.versionsOpts,
          Opts.option[VersionFormat]("format", "Format").withDefault(VersionFormat.Table),
          Opts.option[Int]("max", "Maximum versions to include").withDefault(10)
        ).mapN(runVersions).map(runIO)
      ),
      Command("resolve", "Resolve the artifact")(
        (
          Options.resolveOpt,
          Opts.option[java.nio.file.Path]("extract-to", "Extract to").map(Path.fromNioPath).orNone
        ).mapN(runDownload).map(runIO)),
      Command("deploy", "Deploy the artifact")(
        Options.deployOpt.map(runIO)
      )
    )
}
