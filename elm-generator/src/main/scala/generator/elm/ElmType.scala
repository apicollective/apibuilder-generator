package generator.elm

object ElmType {
  def lookup(typ: String): String = {
    typ match {
      case "string" => "String"
      case other => sys.error(s"No elm type for '$other'")
    }
  }
}
