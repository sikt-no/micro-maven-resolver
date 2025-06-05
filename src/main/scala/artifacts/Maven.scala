package artifacts

import cats.effect.*
import fs2.io.file.{Files, Path}
import io.circe.Codec
import org.apache.maven.artifact.repository.metadata.io.xpp3.MetadataXpp3Reader
import org.eclipse.aether.*
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.deployment.DeployRequest
import org.eclipse.aether.metadata.DefaultMetadata
import org.eclipse.aether.metadata.Metadata.Nature
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.eclipse.aether.resolution.{
  ArtifactDescriptorRequest,
  ArtifactRequest,
  MetadataRequest,
  VersionRequest
}
import org.eclipse.aether.supplier.RepositorySystemSupplier
import org.eclipse.aether.util.artifact.SubArtifact
import org.eclipse.aether.util.repository.AuthenticationBuilder

import java.io.File
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.xml.XML

object Maven {
  case class Module(groupId: GroupId, artifactId: ArtifactId) derives Codec {
    def rendered = s"${groupId}:${artifactId}"
  }

  object Module {
    private val Pattern = "([^: ]+):([^: ]+)".r

    def parse(value: String): Option[Module] =
      value match {
        case Pattern(g, a) => Some(Module(GroupId(g), ArtifactId(a)))
        case _ => None
      }
  }

  case class Coordinates(
      module: Module,
      version: Version,
      classifier: Option[Classifier],
      extension: Option[String]) {
    def toArtifact: DefaultArtifact =
      new DefaultArtifact(
        module.groupId,
        module.artifactId,
        classifier.orNull,
        extension.getOrElse("jar"),
        version.fixMetaVersion
      )

    def rendered = toArtifact.toString
  }

  object Coordinates {
    private val pattern = "([^: ]+):([^: ]+)(:([^: ]*)(:([^: ]+))?)?:([^: ]+)".r.pattern

    def parse(value: String): Option[Coordinates] = {
      val matcher = pattern.matcher(value)
      Option.when(matcher.matches()) {
        Coordinates(
          Module(
            GroupId(matcher.group(1)),
            ArtifactId(matcher.group(2))
          ),
          Version(matcher.group(7)),
          classifier = Option(matcher.group(6)).map(Classifier.apply),
          extension = Option(matcher.group(4))
        )
      }
    }
  }

  case class ResolvedArtifact(coordinates: Coordinates, path: Path)

  case class Versions(
      module: Maven.Module,
      latest: Option[Version],
      release: Option[Version],
      versions: List[Version])
      derives Codec

  def repositoryFor(url: String, up: Option[(username: String, password: String)]) = {
    val pwd = up.map(t =>
      new AuthenticationBuilder().addUsername(t.username).addPassword(t.password).build())
    new RemoteRepository.Builder("maven", "default", url)
      .setAuthentication(pwd.orNull)
      .build()
  }

  val system: Ref[IO, RepositorySystem] = Ref.unsafe {
    val systemsupplier = new RepositorySystemSupplier()
    systemsupplier.get()
  }

  def newSession(system: RepositorySystem, verbose: Boolean) = IO
    .blocking {
      val s = new DefaultRepositorySystemSession()
      if (verbose) {
        s.setRepositoryListener(ConsoleRepositoryListener)
        s.setTransferListener(new ConsoleTransferListener)
      }
      s.setLocalRepositoryManager(
        system.newLocalRepositoryManager(
          s,
          new LocalRepository(new File(Properties.userHome, ".m2/repository"))))
      s
    }

  def versions(repository: RemoteRepository, module: Module, verbose: Boolean) =
    for {
      system <- system.get
      session <- newSession(system, verbose)
      response <- IO.blocking {
        val request = new MetadataRequest(
          new DefaultMetadata(
            module.groupId,
            module.artifactId,
            "maven-metadata.xml",
            Nature.RELEASE))
        request.setRepository(repository)
        val resolved = system.resolveMetadata(session, java.util.List.of(request))
        resolved.asScala.headOption.flatMap(r => Option(r.getMetadata).map(_.getFile.toPath))
      }
      parsed <- IO.blocking {
        response
          .map(path => new MetadataXpp3Reader().read(java.nio.file.Files.newInputStream(path)))
          .map(meta => meta.getVersioning)
          .map(v =>
            Versions(
              module,
              Option(v.getLatest).orElse(Option(v.getRelease)).map(Version.apply),
              Option(v.getRelease).map(Version.apply),
              v.getVersions.asScala.map(Version.apply).toList.reverse
            ))
      }
    } yield parsed

  def resolveVersion(
      repository: RemoteRepository,
      coordinates: Coordinates,
      verbose: Boolean): IO[Option[Version]] =
    for {
      system <- system.get
      session <- newSession(system, verbose)
      response <- IO.blocking {
        val request = new VersionRequest()
        request.setArtifact(coordinates.toArtifact)
        request.setRepositories(java.util.List.of(repository))
        system.resolveVersion(session, request)
      }
    } yield Option(response.getVersion).map(Version.apply)

  def resolve(
      repository: RemoteRepository,
      coordinates: Coordinates,
      verbose: Boolean
  ) =
    for {
      system <- system.get
      session <- newSession(system, verbose)
      response <- IO.blocking {
        val request = new ArtifactRequest()
        request.setArtifact(coordinates.toArtifact)
        request.setRepositories(java.util.List.of(repository))
        system.resolveArtifact(session, request)
      }
    } yield Option.when(response.isResolved)(
      ResolvedArtifact(coordinates, Path.fromNioPath(response.getArtifact.getFile.toPath)))

  def deploy(
      repository: RemoteRepository,
      coordinates: Coordinates,
      path: Path,
      verbose: Boolean
  ): IO[Unit] = {
    def upload(system: RepositorySystem, session: DefaultRepositorySystemSession, pom: Path) =
      IO.blocking {
        val request = new DeployRequest()
        request.setRepository(repository)
        val artifact = coordinates.toArtifact.setFile(path.toNioPath.toFile)
        val pomArtifact = new SubArtifact(artifact, null, "pom", pom.toNioPath.toFile)
        request.setArtifacts(java.util.List.of(artifact, pomArtifact))
        system.deploy(session, request)
      }

    def doesArtifactExistRemote(system: RepositorySystem, session: DefaultRepositorySystemSession) =
      IO.blocking {
        val request =
          new ArtifactDescriptorRequest(coordinates.toArtifact, java.util.List.of(repository), "")
        system.readArtifactDescriptor(session, request)
        true
      }.recover { case _ => false }

    def run(tempDir: Path) = for {
      system <- system.get
      session <- newSession(system, verbose)
      pom <- generatePom(coordinates, tempDir)
      resolved <- doesArtifactExistRemote(system, session)
      response <- {
        if (coordinates.version.isMetaVersion) {
          IO.raiseError(
            new RuntimeException("a meta version (release, latest) is not allowed here"))
        } else {
          val snapshot = coordinates.version.isSnapshot
          if (snapshot) {
            upload(system, session, pom)
          } else if (!resolved) {
            upload(system, session, pom)
          } else {
            IO.consoleForIO.errorln(s"${coordinates} already exists in the remote repository")
          }
        }
      }
    } yield ()
    val fileExistAndNonEmpty =
      Files[IO].getBasicFileAttributes(path).map(attr => attr.isRegularFile && attr.size > 0)

    if (path.extName.stripPrefix(".") == coordinates.extension.getOrElse("jar")) {
      fileExistAndNonEmpty.ifM(
        Files[IO].tempDirectory.use(run),
        IO.raiseError(new RuntimeException("File does not exist or is empty")))
    } else {
      IO.raiseError(
        new RuntimeException(
          s"${path} does not match supplied extension: ${coordinates.extension.getOrElse("jar")}"))
    }
  }

  private def generatePom(coordinates: Maven.Coordinates, tempDir: Path) = IO.blocking {
    val xml =
      <project xmlns="http://maven.apache.org/POM/4.0.0"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        <modelVersion>4.0.0</modelVersion>

        <groupId>{coordinates.module.groupId}</groupId>
        <artifactId>{coordinates.module.artifactId}</artifactId>
        <version>{coordinates.version}</version>
        <packaging>pom</packaging>
      </project>
    val path = tempDir / "pom.xml"
    XML.save(path.toString, xml, xmlDecl = true)
    path
  }
}
