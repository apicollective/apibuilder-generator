package generator.elm

object ElmType {
  def lookup(typ: String): String = {
    typ match {
      case "boolean" => "Bool"
      case "date-iso8601" => "String"
      case "date-time-iso8601" => "String"
      case "decimal" => "Float"
      case "double" => "Float"
      case "integer" => "Int"
//      case "json" => "object"
      case "long" => "Int"
//      case "object" => "object"
      case "string" => "String"
//      case "unit" => "object"
      case "uuid" => "String"
      case other => Names.pascalCase(other)
    }
  }
}
