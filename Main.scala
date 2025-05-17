import cats.data.ValidatedNel
import cats.effect.*
import cats.syntax.all.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.{Argument, Command, Opts}
import coursier.*
import coursier.core.Authentication
import coursier.version.{Latest, Version}
import de.lhns.fs2.compress.*
import de.vandermeer.asciitable.AsciiTable
import de.vandermeer.skb.interfaces.transformers.textformat.TextAlignment
import fs2.io.file.{Files, Path}

import scala.util.control.NonFatal

case class OurVersions(module: Module, latest: Option[Version], candidates: List[Version])
object OurVersions {
  implicit val versionsCodec: JsonValueCodec[OurVersions] =
    JsonCodecMaker.make(CodecMakerConfig.withInlineOneValueClasses(true))
}

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
      MavenRepository(
        repo,
        Some(Authentication(username, password))
      ))

  val groupIdOpt = Opts
    .option[String]("groupId", short = "g", help = "GroupId. Defaults to no.sikt.studieadm")
    .withDefault("no.sikt.studieadm")
    .map(Organization.apply)

  val artifactIdOpt = Opts
    .option[String]("artifactId", short = "a", help = "ArtifactId")
    .map(ModuleName.apply)

  val moduleOpt = (groupIdOpt, artifactIdOpt).mapN((g, a) => Module(g, a))

  val versionOpt = Opts
    .option[String]("version", "Selected version", short = "v")
    .map(
      Version.apply
    )

  val dependencyOpt =
    (moduleOpt, versionOpt).mapN((m, v) => Dependency(m, VersionConstraint.fromVersion(v)))

  def versions(repository: MavenRepository, module: Module) = {
    val versions = Versions()
      .addRepositories(repository)
      .withModule(module)

    IO.fromFuture(
      IO.executionContext
        .map(ec => versions.future()(ec))
    ).map(ver =>
      OurVersions(
        module,
        ver.latest(Latest.Release),
        ver.candidates(Latest.Release).toList.distinct))
  }

  val versionsOpts =
    (repositoryOpt, moduleOpt).mapN(versions)

  def download(repository: MavenRepository, dep: Dependency): IO[Option[Path]] = {
    val fetch = Fetch()
      .addRepositories(repository)
      .addClassifiers(Classifier("compose"))
      .addDependencies(dep)
      .addArtifactTypes(Type.apply("zip"))
    IO.fromFuture(
      IO.executionContext
        .map(ec => fetch.future()(ec))
    ).map(_.headOption.map(f => Path.fromNioPath(f.toPath)))
  }

  val downloadOpt = (repositoryOpt, dependencyOpt).mapN(download)
}

object Main
    extends CommandIOApp(
      "studieadm-maven-resolver",
      "Studieadministrajonens Maven Resolver",
      version = cli.build.BuildInfo.projectVersion.getOrElse("main")) {

  def runVersions(versions: IO[OurVersions], format: VersionFormat): IO[ExitCode] =
    versions
      .flatMap { our =>
        format match {
          case VersionFormat.JSON =>
            IO.blocking(
              writeToStream(our, Console.out)
            )

          case VersionFormat.Table =>
            IO.println {
              val table = {
                val t = new AsciiTable()

                t.addRule()
                t.addRow("latest", "candidates")
                t.addRule()
                t.addRow(
                  our.latest.map(_.repr).getOrElse("none"), {
                    val (toRender, extra) = our.candidates.splitAt(5)
                    val rendered = toRender.map(_.repr).mkString(",")
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
              .flatMap(v => IO.println(v.repr))
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

  def runDownload(action: IO[Option[Path]], extractTo: Option[Path]): IO[ExitCode] =
    action
      .flatMap { file =>
        for {
          file <- IO.fromOption(file)(new RuntimeException("Unable to download dependency"))
          _ <- IO.println(s"Downloaded to $file")
          _ <- extractTo.traverse_(unArchive(file, _))
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
          Opts.option[VersionFormat]("format", "Format").withDefault(VersionFormat.Table))
          .mapN(
            runVersions
          )
      ),
      Command("download", "Download the dependency")(
        (
          Options.downloadOpt,
          Opts.option[java.nio.file.Path]("extract-to", "Extract to").map(Path.fromNioPath).orNone)
          .mapN(runDownload))
    )
}
