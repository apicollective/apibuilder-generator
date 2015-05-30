package models.generator

import java.util.UUID

import org.joda.time.format.ISODateTimeFormat.dateTimeParser
import play.api.libs.json._

import lib.{Datatype, Text}

sealed trait JavaDatatype {
  def apidocType: String

  def shortName: String

  def namespace: Option[String] = None

  def name: String = namespace match {
    case None => shortName
    case Some(ns) => s"$ns.$shortName"
  }

  def asString(varName: String) = s"$varName.toString()"

  def toVariableName = "value"

  def valueFromString(value: String): String = valueFromJson(Json.parse(value))

  def valueFromJson(json: JsValue): String = {
    throw new UnsupportedOperationException(s"Unable to create default value for type $name")
  }
}

object JavaPrimitives {

  case object Boolean extends JavaDatatype {
    override val apidocType = "boolean"
    override val shortName = "boolean"

    override def valueFromJson(json: JsValue) = json.as[scala.Boolean].toString
  }

  case object Double extends JavaDatatype {
    override val apidocType = "double"
    override val shortName = "double"

    override def valueFromJson(json: JsValue) = json.as[scala.Double].toString
  }

  case object Integer extends JavaDatatype {
    override val apidocType = "integer"
    override val shortName = "int"

    override def valueFromJson(json: JsValue) = json.as[scala.Int].toString
  }

  case object Long extends JavaDatatype {
    override val apidocType = "long"
    override val shortName = "long"

    override def valueFromJson(json: JsValue) = json.as[scala.Long].toString
  }

  case object DateIso8601 extends JavaDatatype {
    override val apidocType = "date-iso8601"
    override val namespace = Some("org.joda.time")
    override val shortName = "LocalDate"

    override def valueFromString(value: String) = {
      val dt = dateTimeParser.parseLocalDate(JsString(value).as[String])
      s"new $name(${dt.getYear}, ${dt.getMonthOfYear}, ${dt.getDayOfMonth})"
    }
  }

  case object DateTimeIso8601 extends JavaDatatype {
    override val apidocType = "date-time-iso8601"
    override val namespace = Some("org.joda.time")
    override val shortName = "DateTime"

    override def asString(varName: String): String = {
      s"org.joda.time.format.ISODateTimeFormat.dateTime.print($varName)"
    }

    override def valueFromString(value: String) =
      s"""org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(${JsString(value)})"""
  }

  case object Decimal extends JavaDatatype {
    override val apidocType = "decimal"
    override val namespace = Some("java.lang")
    override val shortName = "BigDecimal"

    override def valueFromJson(json: JsValue) = json.as[scala.BigDecimal].toString()
  }

  case object Object extends JavaDatatype {
    override val apidocType = "object"
    override val namespace = Some("java.util")
    override val shortName = "Map<String, String>"
  }

  case object String extends JavaDatatype {
    override val apidocType = "string"
    override val shortName = "String"

    override def asString(varName: String): String = s"$varName"

    override def valueFromString(value: String) = s""""${JsString(value).as[String]}""""
  }

  case object Unit extends JavaDatatype {
    override val apidocType = "unit"
    override val shortName = "void"
    override def asString(varName: String): String = {
      throw new UnsupportedOperationException(s"Unsupported conversion of type void for $varName")
    }
  }

  case object Uuid extends JavaDatatype {
    override val apidocType = "uuid"
    override val namespace = Some("java.util")
    override val shortName = "UUID"

    override def valueFromString(value: String) = {
      val uuid = JsString(value).as[UUID]
      s"new UUID(${uuid.getMostSignificantBits}, ${uuid.getLeastSignificantBits})"
    }
  }

  case class Model(ns: String, override val shortName: String) extends JavaDatatype {
    override val apidocType = shortName
    override val namespace = Some(ns)

    override val toVariableName = Text.initLowerCase(shortName)
  }

  case class Enum(ns: String, override val shortName: String) extends JavaDatatype {
    override val apidocType = shortName
    override val namespace = Some(ns)

    override def valueFromString(value: String): String = s"$name(${JsString(value)})"

    override val toVariableName = Text.initLowerCase(shortName)
  }

  case class Union(ns: String, override val shortName: String) extends JavaDatatype {
    override val apidocType = shortName
    override val namespace = Some(ns)

    override val toVariableName = Text.initLowerCase(shortName)
  }
}

object JavaCollections {
  sealed abstract class Container(inner: JavaDatatype) extends JavaDatatype {
    override val toVariableName = inner match {
      case _: Container => inner.toVariableName
      case _ => Text.pluralize(inner.toVariableName)
    }
  }

  case class List(inner: JavaDatatype) extends Container(inner) {
    override val apidocType = s"[${inner.apidocType}}]"
    override val namespace = Some("java.util")
    override val shortName = s"List<${inner.name}>"

    override def valueFromJson(json: JsValue) = {
      val defaults = json.as[JsArray].value.map { inner.valueFromJson }
      val initBlock =
        if (defaults.isEmpty) ""
        else s"java.util.Arrays.asList(${defaults.mkString(",")})"

      s"new java.util.ArrayList<${inner.name}>($initBlock)"
    }
  }

  case class Map(inner: JavaDatatype) extends Container(inner) {
    override val apidocType = s"map[${inner.apidocType}]"
    override val namespace = Some("java.util")
    override val shortName = s"Map<String, ${inner.name}>"

    override def valueFromJson(json: JsValue) = {
      import Text._
      val initBlockStatements = json.as[scala.collection.immutable.Map[String, JsValue]].map {
        case (key, value) => s"""put("$key", ${inner.valueFromJson(value)});""".indent(4)
      }

      val initBlock =
        if (initBlockStatements.isEmpty) ""
        else initBlockStatements.mkString(" {{\n", "\n", "\n}}")

      s"new java.util.HashMap<${inner.name}>()$initBlock;"
    }
  }

  case class Option(inner: JavaDatatype) extends JavaDatatype {
    override val apidocType = s"option[${inner.apidocType}}]"
    override val shortName = inner.name

    override val toVariableName = inner.toVariableName
  }
}

object JavaDatatype {

  def apply(defaultNamespace: String, datatype: Datatype): JavaDatatype = datatype match {
    case Datatype.Primitive.Boolean => JavaPrimitives.Boolean
    case Datatype.Primitive.Decimal => JavaPrimitives.Decimal
    case Datatype.Primitive.Integer => JavaPrimitives.Integer
    case Datatype.Primitive.Double => JavaPrimitives.Double
    case Datatype.Primitive.Long => JavaPrimitives.Long
    case Datatype.Primitive.Object => JavaPrimitives.Object
    case Datatype.Primitive.String => JavaPrimitives.String
    case Datatype.Primitive.DateIso8601 => JavaPrimitives.DateIso8601
    case Datatype.Primitive.DateTimeIso8601 => JavaPrimitives.DateTimeIso8601
    case Datatype.Primitive.Uuid => JavaPrimitives.Uuid
    case Datatype.Primitive.Unit => JavaPrimitives.Unit

    case Datatype.Container.List(t) => JavaCollections.List(JavaDatatype(defaultNamespace, t))
    case Datatype.Container.Map(t) => JavaCollections.Map(JavaDatatype(defaultNamespace, t))
    case Datatype.Container.Option(inner) => JavaCollections.Option(JavaDatatype(defaultNamespace, inner))

    case Datatype.UserDefined.Model(name) =>
      val (ns, n) = parseQualifiedName(s"$defaultNamespace.models", name)
      JavaPrimitives.Model(ns, n)

    case Datatype.UserDefined.Enum(name) =>
      val (ns, n) = parseQualifiedName(s"$defaultNamespace.enums", name)
      JavaPrimitives.Enum(ns, n)

    case Datatype.UserDefined.Union(name) =>
      val (ns, n) = parseQualifiedName(s"$defaultNamespace.unions", name)
      JavaPrimitives.Union(ns, n)
  }

  /**
   * If name is a qualified name (as identified by having a dot),
   * parses the name and returns a tuple of (namespace,
   * name). Otherwise, returns (defaultNamespace, name)
   */
  private def parseQualifiedName(defaultNamespace: String, name: String): (String, String) = {
    name.split("\\.").toList match {
      case n :: Nil => (defaultNamespace, JavaUtil.toClassName(n))
      case multiple => 
        val n = multiple.last
        val ns = multiple.dropRight(1).mkString(".")
        (ns, JavaUtil.toClassName(n))
    }
  }
}