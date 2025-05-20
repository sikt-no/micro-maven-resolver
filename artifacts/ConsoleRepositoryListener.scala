package artifacts

import org.eclipse.aether.{AbstractRepositoryListener, RepositoryEvent}
import org.slf4j.Logger

object ConsoleRepositoryListener extends AbstractRepositoryListener {
  override def artifactDeployed(event: RepositoryEvent) =
    Console.err.println("Deployed " + event.getArtifact + " to " + event.getRepository)

  override def artifactDeploying(event: RepositoryEvent) =
    Console.err.println("Deploying " + event.getArtifact + " to " + event.getRepository)

  override def artifactDescriptorInvalid(event: RepositoryEvent) =
    Console.err.println(
      "Invalid artifact descriptor for " + event.getArtifact + ": "
        + event.getException.getMessage
    )

  override def artifactDescriptorMissing(event: RepositoryEvent) =
    Console.err.println("Missing artifact descriptor for " + event.getArtifact)

  override def artifactDownloaded(event: RepositoryEvent) =
    Console.err.println("Downloaded artifact " + event.getArtifact + " from " + event.getRepository)

  override def artifactDownloading(event: RepositoryEvent) =
    Console.err.println(
      "Downloading artifact " + event.getArtifact + " from " + event.getRepository)

  override def artifactInstalled(event: RepositoryEvent) =
    Console.err.println("Installed " + event.getArtifact + " to " + event.getFile)

  override def artifactInstalling(event: RepositoryEvent) =
    Console.err.println("Installing " + event.getArtifact + " to " + event.getFile)

  override def artifactResolved(event: RepositoryEvent) =
    Console.err.println("Resolved artifact " + event.getArtifact + " from " + event.getRepository)

  override def artifactResolving(event: RepositoryEvent) =
    Console.err.println("Resolving artifact " + event.getArtifact)

  override def metadataDeployed(event: RepositoryEvent) =
    Console.err.println("Deployed " + event.getMetadata + " to " + event.getRepository)

  override def metadataDeploying(event: RepositoryEvent) =
    Console.err.println("Deploying " + event.getMetadata + " to " + event.getRepository)

  override def metadataDownloaded(event: RepositoryEvent) =
    Console.err.println("Downloaded metadata " + event.getMetadata + " from " + event.getRepository)

  override def metadataDownloading(event: RepositoryEvent) =
    Console.err.println(
      "Downloading metadata " + event.getMetadata + " from " + event.getRepository)

  override def metadataInstalled(event: RepositoryEvent) =
    Console.err.println("Installed " + event.getMetadata + " to " + event.getFile)

  override def metadataInstalling(event: RepositoryEvent) =
    Console.err.println("Installing " + event.getMetadata + " to " + event.getFile)

  override def metadataInvalid(event: RepositoryEvent) =
    Console.err.println("Invalid metadata " + event.getMetadata)

  override def metadataResolved(event: RepositoryEvent) =
    Console.err.println("Resolved metadata " + event.getMetadata + " from " + event.getRepository)

  override def metadataResolving(event: RepositoryEvent) =
    Console.err.println("Resolving metadata " + event.getMetadata + " from " + event.getRepository)
}
