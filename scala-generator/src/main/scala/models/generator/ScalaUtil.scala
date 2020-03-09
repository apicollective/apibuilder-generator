package scala.generator

import lib.Text._
import lib.generator.GeneratorUtil

import io.apibuilder.spec.v0.models.Deprecation

object ScalaUtil {

  private[this] val ReservedWords = Seq(
    "case", "catch", "class", "def", "do",
    "else", "extends", "false", "final",
    "finally", "for", "forSome", "if",
    "implicit", "import", "lazy", "match",
    "new", "null", "object", "override",
    "package", "private", "protected",
    "return", "sealed", "super", "this",
    "throw", "trait", "try", "true",
    "type", "val", "var", "while",
    "with", "yield"
  ).toSet

  def extendsClause(unions: Seq[String]): Option[String] = {
    unions match {
      case Nil => None
      case _ => Some(" extends " + unions.sorted.mkString(" with "))
    }
  }

  def trimTrailingWhitespace(text: String): String = {
    text.reverse.dropWhile(_ == ' ').reverse
  }

  def textToComment(text: Seq[String]): String = {
    "/**\n" + text.map { t => trimTrailingWhitespace(s" * $t") }.mkString("\n") + "\n */"
  }

  def textToComment(text: String): String = {
    if (text.trim.isEmpty) {
      ""
    } else {
      textToComment(text.split("\n").toSeq.flatMap(s => GeneratorUtil.splitIntoLines(s).map(_.trim)))
    }
  }

  def fieldsToArgList(fields: Seq[String]): Option[String] = {
    if (fields.isEmpty) {
      None
    } else {
      Some(fields.map(_.indentString()).mkString("\n", ",\n", "\n"))
    }
  }

  def isKeyword(value: String): Boolean = {
    ReservedWords.contains(value)
  }

  def quoteNameIfKeyword(name: String): String = {
    if (isKeyword(name) || needsQuoting(name)) {
      "`" + name + "`"
    } else {
      name
    }
  }

  def needsQuoting(name: String): Boolean = {
    name.indexOf("[") >= 0
  }

  def toClassName(name: String): String = {
    val baseName =lib.Text.safeName(
      if (name == name.toUpperCase) {
        lib.Text.initCap(lib.Text.splitIntoWords(name).map(_.toLowerCase)).mkString("")
      } else {
        lib.Text.initCap(snakeToCamelCase(name))
      }
    )

    ScalaUtil.quoteNameIfKeyword(baseName)
  }

  def toVariable(name: String): String = {
    initLowerCase(snakeToCamelCase(name))
  }

  def wrapInQuotes(value: String): String = {
    // TODO: Quote values if needed
    s""""$value""""
  }

  def scalaDefault(value: String, datatype: ScalaDatatype): String = try {
    datatype match {
      case ScalaDatatype.Option(inner) => s"Some(${inner.default(value)})"
      case _ => datatype.default(value)
    }

  } catch {
    case e: Exception => {
      throw new RuntimeException(s"parsing default `$value` for datatype $datatype", e)
    }
  }

  def deprecationString(deprecation: Option[Deprecation]): String = {
    deprecation.map { dep =>
      val description = dep.description.map { desc => s"""("$desc")""" }.getOrElse("")
      s"@deprecated$description "
    }.getOrElse("")
  }

}

