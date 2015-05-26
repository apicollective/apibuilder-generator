package scala.generator

import java.util.UUID

import lib.Datatype
import lib.Text._
import lib.generator.GeneratorUtil
import play.api.libs.json._
import org.joda.time.DateTime

object ScalaUtil {

  private val ReservedWords = Seq(
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

  def extendsClause(names: Seq[String]): Option[String] = {
    names match {
      case Nil => None
      case unions => Some("extends " + names.sorted.mkString(" with "))
    }
  }

  def textToComment(text: Seq[String]): String = {
    "/**\n * " + text.mkString("\n * ") + "\n */"
  }

  def textToComment(text: String): String = {
    if (text.trim.isEmpty) {
      ""
    } else {
      textToComment(GeneratorUtil.splitIntoLines(text))
    }
  }

  def fieldsToArgList(fields: Seq[String]): Option[String] = {
    if (fields.isEmpty) {
      None
    } else {
      Some(fields.map(_.indent).mkString("\n", ",\n", "\n"))
    }
  }

  def quoteNameIfKeyword(name: String): String = {
    if (ReservedWords.contains(name)) {
      "`" + name + "`"
    } else {
      name
    }
  }

  def toClassName(name: String) = {
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
    datatype.default(value)
  } catch {
    case e: Exception => {
      e.printStackTrace(System.err)
      throw new RuntimeException(s"parsing default `$value` for datatype $datatype", e)
    }
  }
}

