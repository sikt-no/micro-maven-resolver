package artifacts

import cats.effect.{IO, Resource}
import com.reposilite.token.AccessTokenType
import com.reposilite.token.api.{CreateAccessTokenRequest, SecretType}
import com.reposilite.{Reposilite, ReposiliteFactory, ReposiliteParameters}
import fs2.io.file.Path
import munit.{AnyFixture, CatsEffectSuite}

import java.net.ServerSocket

class IntegrationTest extends CatsEffectSuite {
  val mavenRepo = ResourceSuiteLocalFixture(
    "maven",
    for {
      temp <- fs2.io.file.Files[IO].tempDirectory
      repo <- Resource.make(IntegrationTest.makeRepo(temp))(repo => IO.blocking(repo.shutdown()))
    } yield repo
  )

  override val munitFixtures: Seq[AnyFixture[?]] = List(
    mavenRepo
  )

  test("deploy") {
    val repo = mavenRepo()
    val repository = Maven.repositoryFor(
      s"http://localhost:${repo.getParameters.getPort}/releases",
      Some((username = "admin", password = "token")))
    val coordinates = Maven.Coordinates.parse("com.example:example:scala:sources:0.1.0").get

    val action = for {
      _ <- Maven.deploy(
        repository,
        coordinates,
        Path.apply("project.scala"),
        verbose = false
      )
      versions <- Maven.versions(repository, coordinates.module, verbose = false)
    } yield versions
    action.assertEquals(
      Some(
        Maven
          .Versions(
            coordinates.module,
            Some(Version("0.1.0")),
            Some(Version("0.1.0")),
            List(Version("0.1.0")))))
  }
}

object IntegrationTest {
  def makeRepo(tempDir: Path): IO[Reposilite] = nextAvailablePort
    .use(IO.pure)
    .flatMap(port =>
      IO.delay {
        val params = new ReposiliteParameters()
        params.setPort(port)
        params.setWorkingDirectory(tempDir.toNioPath)
        params.setPluginDirectory((tempDir / "plugins").toNioPath)
        params.setUsageHelpRequested(false)
        params.setLevel("WARN")
        params.setTestEnv(true)
        params.setLocalConfigurationPath((tempDir / "local").toNioPath)
        params.setTokens(
          java.util.List.of(
            new CreateAccessTokenRequest(
              AccessTokenType.TEMPORARY,
              "admin",
              SecretType.RAW,
              "token")))
        val repo = ReposiliteFactory.INSTANCE.createReposilite(params)
        repo.launch().fold(IO.pure, IO.raiseError)
      }.flatten)

  def nextAvailablePort = Resource
    .make {
      IO.blocking(new ServerSocket(0)).map(s => (s, s.getLocalPort))
    }((socket, _) => IO.blocking(socket.close()))
    .map(_._2)
}
