package generator.elm

import lib.Text

object Names {

  private[this] def withNamespace(imports: Imports, name: String)(f: String => String): String = {
    NamespaceParser.parse(name) match {
      case ParsedName.Local(name) => f(name)
      case ParsedName.Imported(namespace, name) => {
        imports.addExposingAll(s"Generated.${Names.pascalCase(namespace)}")
        f(name)
      }
    }
  }

  def camelCase(imports: Imports, name: String): String = withNamespace(imports, name)(camelCase)

  def pascalCase(name: String): String = maybeQuote(Text.pascalCase(name))

  def camelCase(name: String): String = maybeQuote(Text.snakeToCamelCase(name))

  def wrapInQuotes(name: String): String = {
    // TODO: Escape
    s"\"$name\""
  }

  def maybeQuote(name: String): String = {
    if (Keywords.contains(name)) {
      name + "_"
    } else {
      name
    }
  }

  // https://github.com/elm/compiler/blob/d07679322ef5d71de1bd2b987ddc660a85599b87/compiler/src/Parse/Primitives/Keyword.hs#L3-L12
  private[this] val Keywords: Set[String] = Set(
    "type",
    "alias",
    "port",
    "if",
    "then",
    "else",
    "case",
    "of",
    "let",
    "in",
    "infex",
    "left",
    "right",
    "non",
    "module",
    "import",
    "exposing",
    "as",
    "where",
    "effect",
    "command",
    "subscription",
    "jsonTrue",
    "jsonFalse",
    "jsonNull"
  )
}
