package scala.generator

import lib.Text._
import lib.generator.{GeneratorUtil, NamespaceParser, ParsedName}
import io.apibuilder.spec.v0.models.Deprecation

sealed trait ScalaClassName {
  def className: String
  def qualifiedName: String
}
object ScalaClassName {
  case class Local(override val className: String) extends ScalaClassName {
    override def qualifiedName: String = className
  }
  case class Imported(ns: String, override val className: String) extends ScalaClassName {
    override def qualifiedName: String = s"$ns.models.$className"
  }
}

object ScalaUtil {

  private val ReservedWords = Seq(
    "abstract",
    "as" /* Scala 3 */,
    "case", "catch", "class", "def",
    "derives" /* Scala 3 */,
    "do",
    "else",
    "end" /* Scala 3 */,
    "enum" /* Scala 3 */,
    "export" /* Scala 3 */,
    "extends",
    "extension" /* Scala 3 */,
    "false", "final",
    "finally", "for", "forSome",
    "given" /* Scala 3 */,
    "if",
    "implicit", "import",
    "inline" /* Scala 3 */,
    "lazy", "match",
    "new", "null", "object",
    "opaque" /* Scala 3 */,
    "open" /* Scala 3 */,
    "override",
    "package", "private", "protected",
    "return", "sealed", "super",
    "then" /* Scala 3 */,
    "this",
    "throw", "trait",
    "transparent" /* Scala 3 */,
    "try", "true",
    "type",
    "using" /* Scala 3 */,
    "val", "var", "while",
    "with", "yield"
  ).toSet

  def extendsClause(
    className: String,
    interfaces: Seq[ScalaClassName],
    unions: Seq[String],
  ): Option[String] = {
    extendsTypes(className, interfaces, unions).toList match {
      case Nil => None
      case all => Some(" extends " + all.distinct.sorted.mkString(" with "))
    }
  }

  def extendsTypes(
    className: String,
    interfaces: Seq[ScalaClassName],
    unions: Seq[String],
  ): Seq[String] = {
    (interfaces ++ unions.map(ScalaClassName.Local)).toList
      .filterNot(_.qualifiedName == className)
      .map {
        case n: ScalaClassName.Local => n.className
        case n: ScalaClassName.Imported => n.qualifiedName
      }
      .distinct.sorted
  }

  private def trimTrailingWhitespace(text: String): String = {
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

  def toClassName2(name: String): ScalaClassName = {
    println(s"toClassName2: $name")
    NamespaceParser.parse(name) match {
      case ParsedName.Local(n) => ScalaClassName.Local(toClassName(n))
      case ParsedName.Imported(ns, _, n) => ScalaClassName.Imported(Namespaces(ns).base, toClassName(n))
    }
  }

  def toClassName(name: String): String = {
    val baseName = lib.Text.safeName(
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
