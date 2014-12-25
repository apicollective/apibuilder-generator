package generator

import models.Container
import lib.Datatype
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

  def textToComment(text: String) = {
    if (text.trim.isEmpty) {
      ""
    } else {
      "/**\n * " + GeneratorUtil.splitIntoLines(text).mkString("\n * ") + "\n */"
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

  def packageName(serviceName: String): String = {
   lib.Text.safeName(serviceName).toLowerCase
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
    dt.types match {
      case single :: Nil => {
        single match {
          case Type(TypeKind.Primitive, name) => ScalaUtil.toDefaultVariable(multiple = multiple)
          case Type(TypeKind.Model, name) => ScalaUtil.toVariable(name, multiple = multiple)
          case Type(TypeKind.Enum, name) => ScalaUtil.toVariable(name, multiple = multiple)
        }
      }
      case multiple => {
        sys.error("TODO: UNION TYPE")
      }
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

