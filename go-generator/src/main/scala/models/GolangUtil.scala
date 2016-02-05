package golang.models

import lib.Text._

object GolangUtil {

  // TODO: this is the java list. Update for golang
  private val ReservedWords = Set(
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
    "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto",
    "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new",
    "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized",
    "this", "throw", "throws", "transient", "try", "void", "volatile", "while")

  def quoteNameIfKeyword(value: String): String = {
    // TODO
    value
  }

  def textToComment(text: String): String = {
    textToComment(Seq(text))
  }

  def textToComment(text: Seq[String]): String = {
    "/**\n * " + text.mkString("\n * ") + "\n */"
  }

  def wrapInQuotes(value: String) = {
    s""""$value""""
  }

  /**
    * returns a safe variable name with leading letter in upper case
    */
  def publicName(name: String) = {
    val baseName =safeName(
      if (name == name.toUpperCase) {
       initCap(splitIntoWords(name).map(_.toLowerCase)).mkString("")
      } else {
        initCap(snakeToCamelCase(name))
      }
    )

    quoteNameIfKeyword(baseName)
  }

  /**
    * returns a safe variable name with leading letter in lower case
    */
  def privateName(name: String): String = {
    initLowerCase(publicName(name))
  }

  
}
