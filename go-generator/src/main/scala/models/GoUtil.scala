package go.models

import io.apibuilder.spec.v0.models.Resource
import lib.generator.GeneratorUtil
import lib.Text.*

object GoUtil {

  // See
  //   https://golang.org/ref/spec#Keywords
  //   https://golang.org/ref/spec#Predeclared_identifiers
  private val ReservedWords = Set(
    "break", "default", "func", "interface", "select", "case", "defer", "go",
    "map", "struct", "chan", "else", "goto", "package", "switch", "const", "fallthrough",
    "if", "range", "type", "continue", "for", "import", "return", "var",
    "bool", "byte", "complex64", "complex128", "error", "float32", "float64", "int", "int8",
    "int16", "int32", "int64", "rune", "string", "uint", "uint8", "uint16", "uint32", "uint64",
    "uintptr", "true", "false", "iota", "nil", "append", "cap", "close", "complex", "copy",
    "delete", "imag", "len", "make", "new", "panic", "print", "println", "real", "recover"
  )

  def quoteNameIfKeyword(value: String): String = {
    ReservedWords.contains(value.trim) match {
      case false => value
      case true => s"${value}_"
    }
  }

  def textToComment(text: Option[String]): String = {
    text match {
      case None => ""
      case Some(v) => textToComment(GeneratorUtil.splitIntoLines(v))
    }
  }

  def textToSingleLineComment(text: Option[String]): String = {
    text match {
      case None => ""
      case Some(v) => s"// ${v.trim}\n"
    }
  }

  /**
    * Returns comment, including a trailing newline
    */
  def textToComment(text: Seq[String]): String = {
    "/**\n * " + text.mkString("\n * ") + "\n */\n"
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
    quoteNameIfKeyword(
      initLowerCase(publicName(name))
    )
  }

  def packageName(name: String): String = {
    privateName(name).toLowerCase
  }

  def methodName(resource: Resource): String = {
    publicName(resource.plural)
  }
  
}
