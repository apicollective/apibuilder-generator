package generator

import models.Container
import lib.{Datatype, Type, Kind}
import lib.Text._

object ScalaUtil {

  private val Keywords = Seq("case", "catch", "class", "def", "do",
                             "else", "extends", "false", "final",
                             "finally", "for", "forSome", "if",
                             "implicit", "import", "lazy", "match",
                             "new", "null", "object", "override",
                             "package", "private", "protected",
                             "return", "sealed", "super", "this",
                             "throw", "trait", "try", "true",
                             "type", "val", "var", "while",
                             "with", "yield").toSet

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
    if (Keywords.contains(name)) {
      "`" + name + "`"
    } else {
      name
    }
  }

  def toClassName(
    name: String,
    multiple: Boolean = false
  ) = {
    val baseName =lib.Text.safeName(
      if (name == name.toUpperCase) {
       lib.Text.initCap(lib.Text.splitIntoWords(name).map(_.toLowerCase)).mkString("")
      } else {
       lib.Text.initCap(snakeToCamelCase(name))
      }
    )

    ScalaUtil.quoteNameIfKeyword(
      if (multiple) {
       lib.Text.pluralize(baseName)
      } else {
        baseName
      }
    )

  }

  def toVariable(dt: Datatype): String = {
    val multiple = Container(dt).multiple
    dt.`type` match {
      case Type(Kind.Primitive, _) => ScalaUtil.toDefaultVariable(multiple = multiple)
      case Type(Kind.Model | Kind.Enum | Kind.Union, name) => ScalaUtil.toVariable(name, multiple = multiple)
    }
  }

  def toVariable(
    name: String,
    multiple: Boolean = false
  ): String = {
   lib.Text.initLowerCase(toClassName(name, multiple))
  }

  def toDefaultClassName(
    multiple: Boolean = false
  ): String = toClassName("value", multiple = multiple)

  def toDefaultVariable(
    multiple: Boolean = false
  ): String = toVariable("value", multiple = multiple)

  def wrapInQuotes(value: String): String = {
    // TODO: Quote values if needed
    s""""$value""""
  }
}

