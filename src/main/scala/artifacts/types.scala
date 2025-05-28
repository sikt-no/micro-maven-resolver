package artifacts
import io.circe.Codec

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

  extension (v: Version) {
    def isSnapshot = v.endsWith("-SNAPSHOT")
  }
}

opaque type Classifier <: String = String
object Classifier {
  def apply(in: String): Classifier = in
  given Codec[Classifier] = Codec.implied[String]
}
