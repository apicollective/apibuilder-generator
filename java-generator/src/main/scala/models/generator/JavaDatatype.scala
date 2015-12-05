package models.generator

import java.util.UUID

import org.joda.time.format.ISODateTimeFormat.dateTimeParser
import play.api.libs.json._

import lib.generator.Text
import lib.generator.Datatype

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

// Namespace these to make it easier to refer to them without conflicting with Scala primitive types, etc
object JavaDatatypes {
  sealed trait NativeDatatype extends JavaDatatype

  case object Boolean extends NativeDatatype {
    override val apidocType = "boolean"
    override val shortName = "boolean"

    override def valueFromJson(json: JsValue) = json.as[scala.Boolean].toString
  }

  case object Double extends NativeDatatype {
    override val apidocType = "double"
    override val shortName = "double"

    override def valueFromJson(json: JsValue) = json.as[scala.Double].toString
  }

  case object Integer extends NativeDatatype {
    override val apidocType = "integer"
    override val shortName = "int"

    override def valueFromJson(json: JsValue) = json.as[scala.Int].toString
  }

  case object Long extends NativeDatatype {
    override val apidocType = "long"
    override val shortName = "long"

    override def valueFromJson(json: JsValue) = json.as[scala.Long].toString
  }

  case object DateIso8601 extends NativeDatatype {
    override val apidocType = "date-iso8601"
    override val namespace = Some("org.joda.time")
    override val shortName = "LocalDate"

    override def valueFromString(value: String) = {
      val dt = dateTimeParser.parseLocalDate(JsString(value).as[String])
      s"new $name(${dt.getYear}, ${dt.getMonthOfYear}, ${dt.getDayOfMonth})"
    }
  }

  case object DateTimeIso8601 extends NativeDatatype {
    override val apidocType = "date-time-iso8601"
    override val namespace = Some("org.joda.time")
    override val shortName = "DateTime"

    override def asString(varName: String): String = {
      s"org.joda.time.format.ISODateTimeFormat.dateTime.print($varName)"
    }

    override def valueFromString(value: String) =
      s"""org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(${JsString(value)})"""
  }

  case object Decimal extends NativeDatatype {
    override val apidocType = "decimal"
    override val namespace = Some("java.math")
    override val shortName = "BigDecimal"

    override def valueFromJson(json: JsValue) = json.as[scala.BigDecimal].toString()
  }

  case object Object extends NativeDatatype {
    override val apidocType = "object"
    override val namespace = Some("java.util")
    override val shortName = "Map<String, String>"
  }

  case object String extends NativeDatatype {
    override val apidocType = "string"
    override val shortName = "String"

    override def asString(varName: String): String = s"$varName"

    override def valueFromString(value: String) = s""""${JsString(value).as[String]}""""
  }

  case object Unit extends NativeDatatype {
    override val apidocType = "unit"
    override val shortName = "void"

    override def asString(varName: String): String = {
      throw new UnsupportedOperationException(s"Unsupported conversion of type void for $varName")
    }
  }

  case object Uuid extends NativeDatatype {
    override val apidocType = "uuid"
    override val namespace = Some("java.util")
    override val shortName = "UUID"

    override def valueFromString(value: String) = {
      val uuid = JsString(value).as[UUID]
      s"new UUID(${uuid.getMostSignificantBits}, ${uuid.getLeastSignificantBits})"
    }
  }

  sealed trait UserDefinedDatatype extends JavaDatatype

  case class Model(override val namespace: scala.Option[String], override val shortName: String) extends UserDefinedDatatype {
    override val apidocType = shortName

    override val toVariableName = Text.initLowerCase(shortName)
  }

  case class Enum(override val namespace: scala.Option[String], override val shortName: String) extends UserDefinedDatatype {
    override val apidocType = shortName

    override def valueFromString(value: String): String = s"$name.valueOf(${JsString(value)})"

    override val toVariableName = Text.initLowerCase(shortName)
  }

  case class Union(override val namespace: scala.Option[String], override val shortName: String) extends UserDefinedDatatype {
    override val apidocType = shortName

    override val toVariableName = Text.initLowerCase(shortName)
  }

  sealed abstract class ContainerDatatype(inner: JavaDatatype) extends JavaDatatype {
    override val toVariableName = inner match {
      case _: ContainerDatatype => inner.toVariableName
      case _ => Text.pluralize(inner.toVariableName)
    }
  }

  case class List(inner: JavaDatatype) extends ContainerDatatype(inner) {
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

  case class Map(inner: JavaDatatype) extends ContainerDatatype(inner) {
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

  def apply(datatype: Datatype): JavaDatatype = datatype match {
    case Datatype.Primitive.Boolean => JavaDatatypes.Boolean
    case Datatype.Primitive.Decimal => JavaDatatypes.Decimal
    case Datatype.Primitive.Integer => JavaDatatypes.Integer
    case Datatype.Primitive.Double => JavaDatatypes.Double
    case Datatype.Primitive.Long => JavaDatatypes.Long
    case Datatype.Primitive.Object => JavaDatatypes.Object
    case Datatype.Primitive.String => JavaDatatypes.String
    case Datatype.Primitive.DateIso8601 => JavaDatatypes.DateIso8601
    case Datatype.Primitive.DateTimeIso8601 => JavaDatatypes.DateTimeIso8601
    case Datatype.Primitive.Uuid => JavaDatatypes.Uuid
    case Datatype.Primitive.Unit => JavaDatatypes.Unit

    case Datatype.Container.List(inner) => JavaDatatypes.List(JavaDatatype(inner))
    case Datatype.Container.Map(inner) => JavaDatatypes.Map(JavaDatatype(inner))
    case Datatype.Container.Option(inner) => JavaDatatypes.Option(JavaDatatype(inner))

    case Datatype.UserDefined.Model(name) =>
      val (ns, n) = parseQualifiedName(name)
      JavaDatatypes.Model(ns, n)

    case Datatype.UserDefined.Enum(name) =>
      val (ns, n) = parseQualifiedName(name)
      JavaDatatypes.Enum(ns, n)

    case Datatype.UserDefined.Union(name) =>
      val (ns, n) = parseQualifiedName(name)
      JavaDatatypes.Union(ns, n)
  }

  /**
   * If name is a qualified name (as identified by having a dot),
   * parses the name and returns a tuple of (namespace,
   * name). Otherwise, returns (None, name)
   */
  private def parseQualifiedName(name: String): (Option[String], String) = {
    name.split("\\.").toList match {
      case n :: Nil => (None, JavaUtil.toClassName(n))
      case multiple =>
        val n = multiple.last
        val ns = multiple.dropRight(1).mkString(".")
        (Some(ns), JavaUtil.toClassName(n))
    }
  }
}
