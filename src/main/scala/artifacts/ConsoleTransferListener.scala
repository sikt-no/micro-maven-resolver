package artifacts

import org.eclipse.aether.transfer.{AbstractTransferListener, TransferEvent, TransferResource}

import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

object ConsoleTransferListener extends AbstractTransferListener {
  private val downloads = new ConcurrentHashMap[TransferResource, Long]()

  private var lastLength: Int = 0

  override def transferInitiated(event: TransferEvent) = {
    val message = if (event.getRequestType == TransferEvent.RequestType.PUT) "Uploading" else "Downloading"

    Console.err.println(message + ": " + event.getResource.getRepositoryUrl + event.getResource.getResourceName)
  }

  override def transferProgressed(event: TransferEvent) = {
    val resource = event.getResource
    downloads.put(resource, event.getTransferredBytes)

    val buffer = new StringBuilder(64)

    import scala.collection.JavaConverters.*
    for (entry <- downloads.entrySet().asScala) {
      val total    = entry.getKey.getContentLength
      val complete = entry.getValue

      buffer.append(getStatus(complete, total)).append("  ")
    }

    val padding = lastLength - buffer.length
    lastLength = buffer.length
    pad(buffer, padding)
    buffer.append('\r')

    Console.err.print(buffer.toString())
  }

  override def transferSucceeded(event: TransferEvent) = {
    transferCompleted(event);

    val resource      = event.getResource
    val contentLength = event.getTransferredBytes
    if (contentLength >= 0) {
      val t   = if (event.getRequestType == TransferEvent.RequestType.PUT) "Uploaded" else "Downloaded"
      val len = if (contentLength >= 1024) toKB(contentLength) + " KB" else contentLength + " B"

      var throughput = ""
      val duration   = System.currentTimeMillis() - resource.getTransferStartTime
      if (duration > 0) {
        val format   = new DecimalFormat("0.0", new DecimalFormatSymbols(Locale.ENGLISH))
        val kbPerSec = (contentLength / 1024.0) / (duration / 1000.0)
        throughput = " at " + format.format(kbPerSec) + " KB/sec"
      }

      Console.err.println(
        t + ": " + resource.getRepositoryUrl + resource.getResourceName + " (" + len
          + throughput + ")"
      )
    }
  }

  override def transferFailed(event: TransferEvent) = {
    transferCompleted(event)

    Console.err.println(event.getException.getMessage)
  }

  private def transferCompleted(event: TransferEvent) = {
    downloads.remove(event.getResource)

    val buffer = new StringBuilder(64)
    pad(buffer, lastLength)
    buffer.append('\r')
    Console.err.println(buffer.toString())
  }

  override def transferCorrupted(event: TransferEvent) = {
    Console.err.println(event.getException.getMessage)
  }

  private def pad(buffer: StringBuilder, spaces: Int) = {
    var thespaces = spaces
    val block     = "                                        "
    while (thespaces > 0) {
      val n = math.min(thespaces, block.length())
      buffer.appendAll(block.toCharArray, 0, n)
      thespaces -= n
    }
  }

  private def toKB(bytes: Long) = (bytes + 1023) / 1024

  private def getStatus(complete: Long, total: Long): String = {
    if (total >= 1024) {
      toKB(complete) + "/" + toKB(total) + " KB "
    } else if (total >= 0) {
      complete + "/" + total + " B "
    } else if (complete >= 1024) {
      toKB(complete) + " KB "
    } else {
      complete + " B "
    }
  }

}