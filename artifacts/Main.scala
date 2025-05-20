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
    Opts.env[String]("ARTIFACTORY_USERNAME", help = "username for resolving from artifactory")
  val passwordOpt =
    Opts.env[String]("ARTIFACTORY_PASSWORD", help = "password for resolving from artifactory")

  val repositoryNameOpt =
    Opts
      .env[String]("ARTIFACTORY_REPOSITORY", help = "Lookup in which repository")
      .withDefault("libs-releases")
      .map(repo => s"https://artifactory.sikt.no/artifactory/${repo}/")

  val repositoryOpt =
    (usernameOpt, passwordOpt, repositoryNameOpt).mapN((username, password, repo) =>
      new RemoteRepository.Builder("artifactory", "default", repo)
        .setAuthentication(
          new AuthenticationBuilder().addUsername(username).addPassword(password).build())
        .build())

  given Argument[Maven.Module] = new Argument[Maven.Module] {
    override def read(string: String): ValidatedNel[String, Maven.Module] =
      Maven.Module.parse(string) match {
        case Some(module) => module.validNel
        case None =>
          s"Invalid module: '${string}', expected format is <groupId>:<artifactId>".invalidNel
      }

    override def defaultMetavar: String = "coordinates"
  }

  val moduleOpt = Opts.option[Maven.Module](
    "coordinates",
    "Coordinates for maven format is <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>")

  given Argument[Maven.Coordinates] = new Argument[Maven.Coordinates] {
    override def read(string: String): ValidatedNel[String, Maven.Coordinates] =
      Coordinates.parse(string) match {
        case Some(coords) => coords.validNel
        case None =>
          s"Invalid coords: '${string}', expected format is <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>".invalidNel
      }

    override def defaultMetavar: String = "coordinates"
  }

  val coordinatesOpt = Opts.option[Maven.Coordinates](
    "coordinates",
    "Coordinates for maven format is <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>")

  val versionsOpts =
    (repositoryOpt, moduleOpt).mapN((repo, module) => Maven.versions(repo, module))

  def download(
      repository: RemoteRepository,
      coordinates: Maven.Coordinates
  ): IO[Option[Maven.ResolvedArtifact]] =
    Maven.resolve(repository, coordinates)

  val downloadOpt = (repositoryOpt, coordinatesOpt).mapN(download)
}

object Main
    extends CommandIOApp(
      "studieadm-maven-resolver",
      "Studieadministrajonens Maven Resolver",
      version = cli.build.BuildInfo.projectVersion.getOrElse("main")) {

  def runVersions(
      versions: IO[Option[Maven.Versions]],
      format: VersionFormat,
      maxVersions: Int): IO[ExitCode] =
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
                t.addRow("latest", "candidates")
                t.addRule()
                t.addRow(
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
              .flatMap(v => IO.println(v.toString))
        }
      }
      .as(ExitCode.Success)
      .recoverWith { case NonFatal(e) =>
        IO.consoleForIO.errorln(s"Error occured: ${e.getMessage}").as(ExitCode(1))
      }

  def unArchive(archive: Path, writeTo: Path)(implicit files: Files[IO]): IO[Unit] =
    files
      .readAll(archive)
      .through(ZipUnarchiver.make[IO]().unarchive)
      .flatMap { case (entry, data) =>
        data.through(files.writeAll(writeTo.resolve(entry.name)))
      }
      .compile
      .drain

  def runDownload(
      action: IO[Option[Maven.ResolvedArtifact]],
      extractTo: Option[Path]): IO[ExitCode] =
    action
      .flatMap { maybeResolved =>
        for {
          resolved <- IO.fromOption(maybeResolved)(
            new RuntimeException("Unable to download dependency"))
          _ <- IO.println(s"Downloaded to ${resolved.path}")
          _ <- extractTo.traverse_(unArchive(Path.fromNioPath(resolved.path), _))
        } yield ExitCode.Success
      }
      .recoverWith { case NonFatal(e) =>
        IO.consoleForIO.errorln(s"Error occured: ${e.getMessage}").as(ExitCode(1))
      }

  override def main: Opts[IO[ExitCode]] =
    Opts.subcommands(
      Command("versions", "Versions")(
        (
          Options.versionsOpts,
          Opts.option[VersionFormat]("format", "Format").withDefault(VersionFormat.Table),
          Opts.option[Int]("max", "Maximum versions to include").withDefault(10)
        ).mapN(
          runVersions
        )
      ),
      Command("download", "Download the dependency")(
        (
          Options.downloadOpt,
          Opts.option[java.nio.file.Path]("extract-to", "Extract to").map(Path.fromNioPath).orNone
        ).mapN(runDownload))
    )
}
