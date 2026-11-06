package artifacts

import cats.effect.IO
import com.dimafeng.testcontainers.GenericContainer
import com.dimafeng.testcontainers.munit.TestContainerForEach
import fs2.io.file.Path
import munit.CatsEffectSuite
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy

class IntegrationTest extends CatsEffectSuite with TestContainerForEach {
  val tempLocal = ResourceFunFixture(fs2.io.file.Files[IO].tempDirectory)

  override val containerDef: GenericContainer.Def[GenericContainer] =
    GenericContainer.Def(
      dockerImage = "dzikoysk/reposilite:3.5.26",
      env = Map("REPOSILITE_OPTS" -> "--port 9999 --token admin:token"),
      exposedPorts = Seq(9999),
      waitStrategy = new HttpWaitStrategy().forPort(9999).forPath("/")
    )

  tempLocal.test("deploy") { (tmp: Path) =>
    withContainers { (c: Containers) =>
      val repository = Maven.repositoryFor(
        s"http://${c.host}:${c.firstMappedPort}/releases",
        Some((username = "admin", password = "token")))
      val coordinates = Maven.Coordinates.parse("com.example:example:scala:sources:0.1.0").get

      val action = for {
        maven <- Maven.make(tmp)
        _ <- maven.deploy(
          repository,
          coordinates,
          Path.apply("project.scala"),
          verbose = false
        )
        versions <- maven.versions(repository, coordinates, verbose = false)
      } yield versions
      action.assertEquals(
        Some(
          Maven
            .Versions(
              coordinates.module,
              Some(Version("0.1.0")),
              Some(Version("0.1.0")),
              List(Version("0.1.0")),
              None,
              Nil)
        ))
    }
  }

  tempLocal.test("deploy-snapshot") { (tmp: Path) =>
    withContainers { (c: Containers) =>
      val repository = Maven.repositoryFor(
        s"http://${c.host}:${c.firstMappedPort}/releases",
        Some((username = "admin", password = "token")))
      val coordinates =
        Maven.Coordinates.parse("com.example:example:scala:sources:0.1.0-SNAPSHOT").get

      val action = for {
        maven <- Maven.make(tmp)
        _ <- maven.deploy(
          repository,
          coordinates,
          Path.apply("project.scala"),
          verbose = false
        )
        versions <- maven.versions(repository, coordinates, verbose = false)
        resolvedVersion <- maven.resolveVersion(repository, coordinates, verbose = false)
        latest <- maven.resolveVersion(
          repository,
          coordinates.copy(version = Version.Latest),
          verbose = false)
        release <- maven.resolveVersion(
          repository,
          coordinates.copy(version = Version.Release),
          verbose = false)
      } yield (resolvedVersion, latest, release, versions)
      action.map { (version, latest, release, versions) =>
        assertNotEquals(version.get, coordinates.version)
        assertEquals(version, versions.get.snapshot.get.asVersion(coordinates.version))
        assert(release.isEmpty)
        assertEquals(version, latest)
      }
    }
  }
}
