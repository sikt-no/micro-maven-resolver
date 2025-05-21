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
import org.eclipse.aether.resolution.{ArtifactRequest, MetadataRequest}
import org.eclipse.aether.supplier.RepositorySystemSupplier
import org.eclipse.aether.util.artifact.SubArtifact
import org.eclipse.aether.util.repository.AuthenticationBuilder

import java.io.File
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.xml.XML

object Maven {
  opaque type GroupId <: String = String
  object GroupId {
    def apply(in: String): GroupId = in
    given Codec[GroupId] = Codec.implied[String]
  }

  opaque type ArtifactId <: String = String
  object ArtifactId {
    def apply(in: String): ArtifactId = in
    given Codec[ArtifactId] = Codec.implied[String]
  }

  opaque type Version <: String = String
  object Version {
    def apply(in: String): Version = in
    given Codec[Version] = Codec.implied[String]
  }

  opaque type Classifier <: String = String
  object Classifier {
    def apply(in: String): Classifier = in
    given Codec[Classifier] = Codec.implied[String]
  }

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
        version)

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
      latest: Option[Maven.Version],
      versions: List[Maven.Version])
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
        s.setTransferListener(ConsoleTransferListener)
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
              v.getVersions.asScala.map(Version.apply).toList.reverse))
      }
    } yield parsed

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
    def run(tempDir: Path) = for {
      system <- system.get
      session <- newSession(system, verbose)
      pom <- generatePom(coordinates, tempDir)
      response <- IO.blocking {
        val request = new DeployRequest()
        request.setRepository(repository)
        val artifact = coordinates.toArtifact.setFile(path.toNioPath.toFile)
        val pomArtifact = new SubArtifact(artifact, null, "pom", pom.toNioPath.toFile)
        request.setArtifacts(java.util.List.of(artifact, pomArtifact))
        system.deploy(session, request)
      }
    } yield ()

    Files[IO].tempDirectory.use(run)
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
