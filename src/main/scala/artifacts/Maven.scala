package artifacts

import cats.data.OptionT
import cats.effect.*
import cats.implicits.catsSyntaxOptionId
import fs2.io.file.{Files, Path}
import io.circe.Codec
import org.apache.maven.artifact.repository.metadata.Versioning
import org.apache.maven.artifact.repository.metadata.io.xpp3.MetadataXpp3Reader
import org.eclipse.aether.*
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.deployment.DeployRequest
import org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory
import org.eclipse.aether.internal.impl.checksum.{
  DefaultChecksumAlgorithmFactorySelector,
  Md5ChecksumAlgorithmFactory,
  Sha1ChecksumAlgorithmFactory,
  Sha256ChecksumAlgorithmFactory,
  Sha512ChecksumAlgorithmFactory
}
import org.eclipse.aether.metadata.DefaultMetadata
import org.eclipse.aether.metadata.Metadata.Nature
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.eclipse.aether.resolution.{ArtifactDescriptorRequest, ArtifactRequest, MetadataRequest}
import org.eclipse.aether.supplier.RepositorySystemSupplier
import org.eclipse.aether.util.artifact.SubArtifact
import org.eclipse.aether.util.repository.AuthenticationBuilder

import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.xml.XML

object Maven {
  case class Snapshot(timestamp: String, buildNumber: Int) derives Codec {
    def asVersion(version: Version): Option[Version] =
      if version.isSnapshot then {
        Version(s"${version.withoutSnapshot}-${timestamp}-${buildNumber}").some
      } else None
  }
  case class SnapshotVersion(
      classifier: Option[Classifier],
      extension: Option[String],
      version: Version,
      updated: String
  ) derives Codec

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
      versions: List[Version],
      snapshot: Option[Snapshot],
      snapshotVersions: List[SnapshotVersion])
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

  val localRepository = Path(s"${Properties.userHome}/.m2/repository")

  def make(localRepository: Path = Maven.localRepository): IO[Maven] =
    system.modify(system => system -> new Maven(system, localRepository))
}

class Maven(system: RepositorySystem, localRepository: Path) {
  private val layoutFactory = Maven2RepositoryLayoutFactory(
    DefaultChecksumAlgorithmFactorySelector(
      java.util.Map.of(
        Sha512ChecksumAlgorithmFactory.NAME,
        Sha512ChecksumAlgorithmFactory(),
        Sha256ChecksumAlgorithmFactory.NAME,
        Sha256ChecksumAlgorithmFactory(),
        Sha1ChecksumAlgorithmFactory.NAME,
        Sha1ChecksumAlgorithmFactory(),
        Md5ChecksumAlgorithmFactory.NAME,
        Md5ChecksumAlgorithmFactory()
      )
    ))

  def newSession(verbose: Boolean) = IO
    .blocking {
      val s = new DefaultRepositorySystemSession()
      if (verbose) {
        s.setRepositoryListener(ConsoleRepositoryListener)
        s.setTransferListener(new ConsoleTransferListener)
      }
      s.setLocalRepositoryManager(
        system.newLocalRepositoryManager(s, new LocalRepository(localRepository.toNioPath.toFile)))
      s
    }

  def versions(
      repository: RemoteRepository,
      moduleOrCoordinates: Maven.Module | Maven.Coordinates,
      verbose: Boolean) = {
    val (module, version) = moduleOrCoordinates match {
      case m: Maven.Module => (m, None)
      case Maven.Coordinates(m, v, _, _) => (m, Some(v))
    }
    for {
      session <- newSession(verbose)
      response <- IO.blocking {
        val request = (module, version) match {
          case (Maven.Module(gid, aid), Some(version)) if version.isMetaVersion =>
            new MetadataRequest(
              new DefaultMetadata(gid, aid, "maven-metadata.xml", Nature.RELEASE_OR_SNAPSHOT))
          case (Maven.Module(gid, aid), Some(version)) if version.isSnapshot =>
            new MetadataRequest(
              new DefaultMetadata(gid, aid, version, "maven-metadata.xml", Nature.SNAPSHOT))
          case (Maven.Module(gid, aid), _) =>
            new MetadataRequest(new DefaultMetadata(gid, aid, "maven-metadata.xml", Nature.RELEASE))
        }
        request.setRepository(repository)
        val resolved = system.resolveMetadata(session, java.util.List.of(request))
        resolved.asScala.headOption.flatMap(r => Option(r.getMetadata).map(_.getFile.toPath))
      }
      parsed <- IO.blocking {
        response
          .map(path => new MetadataXpp3Reader().read(java.nio.file.Files.newInputStream(path)))
          .map(meta => meta.getVersioning)
          .map(convertVersioning(moduleOrCoordinates, _))
      }
    } yield parsed
  }

  def resolveVersion(
      repository: RemoteRepository,
      coordinates: Maven.Coordinates,
      verbose: Boolean): IO[Option[Version]] =
    OptionT(versions(repository, coordinates, verbose)).subflatMap { versions =>
      if coordinates.version.isLatest then versions.latest
      else if coordinates.version.isRelease then versions.release
      else if (coordinates.version.isSnapshot)
        versions.snapshot.flatMap(s => s.asVersion(coordinates.version))
      else {
        versions.versions.find(v => v == coordinates.version)
      }
    }.value

  def resolveUrl(repository: RemoteRepository, coordinates: Maven.Coordinates, verbose: Boolean) =
    OptionT(resolveVersion(repository, coordinates, verbose)).semiflatMap { v =>
      val newCoords = coordinates.copy(version = v)
      for {
        session <- newSession(verbose)
        url <- IO.blocking {
          val layout = layoutFactory.newInstance(session, repository)
          val baseUrl = {
            val url =
              if repository.getUrl.endsWith("/") then repository.getUrl
              else repository.getUrl + "/";
            java.net.URI.create(url)
          }
          val artifactUrl = layout.getLocation(newCoords.toArtifact, false)
          baseUrl.resolve(artifactUrl)
        }
      } yield url
    }.value

  def resolve(
      repository: RemoteRepository,
      coordinates: Maven.Coordinates,
      verbose: Boolean
  ) =
    for {
      session <- newSession(verbose)
      response <- IO.blocking {
        val request = new ArtifactRequest()
        request.setArtifact(coordinates.toArtifact)
        request.setRepositories(java.util.List.of(repository))
        system.resolveArtifact(session, request)
      }
    } yield Option.when(response.isResolved)(
      Maven.ResolvedArtifact(coordinates, Path.fromNioPath(response.getArtifact.getFile.toPath)))

  def deploy(
      repository: RemoteRepository,
      coordinates: Maven.Coordinates,
      path: Path,
      verbose: Boolean
  ): IO[Unit] = {
    def upload(session: DefaultRepositorySystemSession, pom: Path) =
      IO.blocking {
        val request = new DeployRequest()
        request.setRepository(repository)
        val artifact = coordinates.toArtifact.setFile(path.toNioPath.toFile)
        val pomArtifact = new SubArtifact(artifact, null, "pom", pom.toNioPath.toFile)
        request.setArtifacts(java.util.List.of(artifact, pomArtifact))
        system.deploy(session, request)
      }

    def doesArtifactExistRemote(session: DefaultRepositorySystemSession) =
      IO.blocking {
        val request =
          new ArtifactDescriptorRequest(coordinates.toArtifact, java.util.List.of(repository), "")
        system.readArtifactDescriptor(session, request)
        true
      }.recover { case _ => false }

    def run(tempDir: Path) = for {
      session <- newSession(verbose)
      pom <- generatePom(coordinates, tempDir)
      resolved <- doesArtifactExistRemote(session)
      response <- {
        if (coordinates.version.isMetaVersion) {
          IO.raiseError(
            new RuntimeException("a meta version (release, latest) is not allowed here"))
        } else {
          val snapshot = coordinates.version.isSnapshot
          if (snapshot) {
            upload(session, pom)
          } else if (!resolved) {
            upload(session, pom)
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
  private def convertVersioning(
      moduleOrCoordinates: Maven.Module | Maven.Coordinates,
      versioning: Versioning) = {
    val (module, version, classifier, extension) = moduleOrCoordinates match {
      case m: Maven.Module => (m, None, None, None)
      case Maven.Coordinates(m, v, c, e) => (m, Some(v), c, e)
    }
    val maybeSnapshot =
      Option(versioning.getSnapshot).map(s => Maven.Snapshot(s.getTimestamp, s.getBuildNumber))
    val snapshotVersion =
      maybeSnapshot.flatMap(snapshot => version.flatMap(v => snapshot.asVersion(v)))
    val snapshotVersions = Option(versioning.getSnapshotVersions)
      .getOrElse(java.util.List.of())
      .asScala
      .map(sv =>
        Maven.SnapshotVersion(
          Option(sv.getClassifier).filterNot(_.trim.isEmpty).map(Classifier.apply),
          Some(sv.getExtension),
          Version(sv.getVersion),
          sv.getUpdated))
      .toList
    val versions = versioning.getVersions.asScala.map(Version.apply).toList
    val latestVersion = versions.lastOption
    val latestSnapshot =
      version.flatMap(v =>
        if v.isLatest then
          snapshotVersions
            .sortBy(_.updated)(using summon[Ordering[String]].reverse)
            .find(sn => sn.extension == extension && sn.classifier == classifier)
            .map(sn => sn.version)
        else None)
    val latest = Option(versioning.getLatest)
      .orElse(Option(versioning.getRelease))
      .map(Version.apply)
      .orElse(snapshotVersion)
      .orElse(latestVersion)

    Maven.Versions(
      module,
      latest,
      Option(versioning.getRelease).map(Version.apply),
      versions.reverse,
      maybeSnapshot,
      snapshotVersions
    )
  }

}
