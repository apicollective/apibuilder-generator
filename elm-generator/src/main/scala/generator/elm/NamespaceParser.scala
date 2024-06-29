package generator.elm

sealed trait ParsedName

object ParsedName {
  case class Local(name: String) extends ParsedName
  case class Imported(namespace: String, name: String) extends ParsedName
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

  private val TypeIndicators = Seq("enums", "models", "unions")
  private def isTypeIndicator(value: String): Boolean = TypeIndicators.contains(value)

  def parse(name: String): ParsedName = {
    name.split("\\.").filterNot(isVersion).filterNot(isTypeIndicator).toList match {
      case Nil => sys.error("Failed to parse name")
      case name :: Nil => ParsedName.Local(name = name)
      case multiple => {
        ParsedName.Imported(
          namespace = multiple.dropRight(1).mkString("_"),
          name = multiple.last
        )
      }
    }
  }
}



