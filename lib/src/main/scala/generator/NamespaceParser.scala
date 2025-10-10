package lib.generator

sealed trait ParsedName {
  def name: String
  def qualifiedName: String
}

object ParsedName {
  case class Local(name: String) extends ParsedName {
    override def qualifiedName: String = name
  }
  case class Imported(namespace: String, typ: ObjectType, name: String) extends ParsedName {
    override def qualifiedName: String = s"$namespace.$typ.$name"
  }
}

object NamespaceParser {

  /**
   * returns true if the value is 'v0', 'v1', etc indicating a version number
   */
  def isVersion(value: String): Boolean = {
    if (value.startsWith("v")) {
      value.drop(1).toLongOption.isDefined
    } else {
      false
    }
  }

  def parse(name: String): ParsedName = {
    val parts = name.split("\\.")
    parts.filterNot(ObjectType.fromString(_).isDefined).toList match {
      case Nil => sys.error("Failed to parse name")
      case name :: Nil => ParsedName.Local(name = name)
      case multiple => {
        parts.flatMap(ObjectType.fromString).distinct.toList match {
          case Nil => sys.error(s"Failed to identify type of object from name: $name")
          case typ :: Nil => {
            ParsedName.Imported(
              namespace = multiple.dropRight(1).mkString("."),
              typ = typ,
              name = multiple.last
            )
          }
          case multiple => sys.error(s"Multiple types of object from name: $name: $multiple")
        }
      }
    }
  }
}
