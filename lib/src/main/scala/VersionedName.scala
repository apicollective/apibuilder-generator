package lib

case class VersionedName(
  name: String,
  version: Option[String] = None
) extends Ordered[VersionedName] {

  private[lib] val versionTag = version.map(VersionTag(_))

  val label: String = version match {
    case None => name
    case Some(v) => s"$name:$v"
  }

  override def compare(that: VersionedName): Int = {
    if (versionTag.isEmpty && that.versionTag.isEmpty) {
      0
    } else if (versionTag.isEmpty && that.versionTag.nonEmpty) {
      1
    } else if (versionTag.nonEmpty && that.versionTag.isEmpty) {
      -1
    } else {
     versionTag.get.compare(that.versionTag.get)
    }
  }

}

