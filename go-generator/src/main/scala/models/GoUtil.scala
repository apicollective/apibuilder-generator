package go.models

import com.bryzek.apidoc.spec.v0.models.Resource
import lib.Text._

object GoUtil {

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
    quoteNameIfKeyword(
      safeName(
        if (name == name.toUpperCase) {
          initCap(splitIntoWords(name).map(_.toLowerCase)).mkString("")
        } else {
          initCap(snakeToCamelCase(name))
        }
      )
    )
  }

  /**
    * returns a safe variable name with leading letter in lower case
    */
  def privateName(name: String): String = {
    initLowerCase(publicName(name))
  }

  def packageName(name: String): String = {
    publicName(name).toLowerCase
  }

  def methodName(resource: Resource): String = {
    publicName(resource.plural)
  }
  
}
