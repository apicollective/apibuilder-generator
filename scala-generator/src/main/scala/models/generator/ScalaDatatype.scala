package scala.generator

import io.apibuilder.spec.v0.models.Deprecation
import lib.Datatype
import lib.Text.{appendSpace, initLowerCase}
import play.api.libs.json.*

import java.util.UUID
import scala.util.{Failure, Success, Try}

sealed trait ScalaDatatype {
  def asString(originalVarName: String): String = {
    throw new UnsupportedOperationException(s"unsupported conversion of type $name for var $originalVarName")
  }

  def name: String

  def deprecationString(deprecation: Option[Deprecation]): String =
    ScalaUtil.deprecationString(deprecation)

  def definition(
    originalVarName: String,
    default: Option[String],
    deprecation: Option[Deprecation],
    scalaTypeKind: ScalaTypeKind,
  ): String = {
    val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
    val base = s"${deprecationString(deprecation)}${appendSpace(scalaTypeKind.toString)}$varName: $name"
    default match {
      case None => base
      case Some(d) => s"$base = $d"
    }
  }

  def default(value: String): String = {
    Try {
      Json.parse(value)
    } match {
      case Success(js) => default(js)
      case Failure(_) => default(JsString(value))
    }
  }

  protected def default(json: JsValue): String = {
    throw new UnsupportedOperationException(s"default for type ${name}")
  }

  protected def toBigDecimal(json: JsValue): BigDecimal = {
    json match {
      case v: JsString => {
        if (v.value.matches("0+")) {
          BigDecimal("0")
        } else {
          BigDecimal(json.toString)
        }
      }
      case v: JsNumber => v.value
      case _ => BigDecimal(json.toString)
    }
  }

  def toVariableName: String
}

sealed trait ScalaPrimitive extends ScalaDatatype {
  def apiBuilderType: String
  def shortName: String
  override def asString(originalVarName: String): String = s"${ScalaUtil.quoteNameIfKeyword(originalVarName)}.toString"
  def namespace: Option[String] = None
  def fullName: String = namespace match {
    case None => shortName
    case Some(ns) => s"$ns.$shortName"
  }
  override def name: String = fullName

  override def toVariableName = "value"

  def fromStringValue(value: String): String = s"$fullName(${ScalaUtil.quoteNameIfKeyword(value)})"
}

object ScalaPrimitive {

  case object Boolean extends ScalaPrimitive {
    def apiBuilderType = "boolean"
    def shortName = "Boolean"
    override def fromStringValue(value: String) = s"${ScalaUtil.quoteNameIfKeyword(value)}.toBoolean"
    override protected def default(json: JsValue): String = json.as[scala.Boolean].toString
  }

  case object Double extends ScalaPrimitive {
    def apiBuilderType = "double"
    def shortName = "Double"
    override def fromStringValue(value: String) = s"${ScalaUtil.quoteNameIfKeyword(value)}.toDouble"
    override protected def default(json: JsValue): String = toBigDecimal(json).toDouble.toString
  }

  case object Integer extends ScalaPrimitive {
    def apiBuilderType = "integer"
    def shortName = "Int"
    override def fromStringValue(value: String) = s"${ScalaUtil.quoteNameIfKeyword(value)}.toInt"
    override protected def default(json: JsValue): String = toBigDecimal(json).toInt.toString
  }

  case object Long extends ScalaPrimitive {
    def apiBuilderType = "long"
    def shortName = "Long"
    override def fromStringValue(value: String) = s"${ScalaUtil.quoteNameIfKeyword(value)}.toLong"
    override protected def default(json: JsValue): String = toBigDecimal(json).toLong.toString + 'L'
  }

  sealed trait DateIso8601 extends ScalaPrimitive

  case object DateIso8601Joda extends DateIso8601 {
    override def namespace: Option[String] = Some("_root_.org.joda.time")
    def apiBuilderType = "date-iso8601"
    def shortName = "LocalDate"
    override def asString(originalVarName: String): String = s"_root_.org.joda.time.format.ISODateTimeFormat.date.print(${ScalaUtil.quoteNameIfKeyword(originalVarName)})"
    override def fromStringValue(value: String) = s"_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate(${ScalaUtil.quoteNameIfKeyword(value)})"
    override def default(value: String): String = fromStringValue(ScalaUtil.wrapInQuotes(value))
    override protected def default(json: JsValue): String = default(json.toString)
  }

  case object DateIso8601Java extends DateIso8601 {
    override def namespace: Option[String] = Some("_root_.java.time")
    def apiBuilderType = "date-iso8601"
    def shortName = "LocalDate"
    override def fromStringValue(value: String) = s"_root_.java.time.LocalDate.parse(${ScalaUtil.quoteNameIfKeyword(value)})"
    override def default(value: String): String = fromStringValue(ScalaUtil.wrapInQuotes(value))
  }

  sealed trait DateTimeIso8601 extends ScalaPrimitive

  case object DateTimeIso8601Joda extends DateTimeIso8601 {
    override def namespace: Option[String] = Some("_root_.org.joda.time")
    def apiBuilderType = "date-time-iso8601"
    def shortName = "DateTime"
    override def asString(originalVarName: String): String = s"_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(${ScalaUtil.quoteNameIfKeyword(originalVarName)})"
    override def fromStringValue(value: String) = s"_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(${ScalaUtil.quoteNameIfKeyword(value)})"
    override def default(value: String): String = fromStringValue(ScalaUtil.wrapInQuotes(value))
    override protected def default(json: JsValue): String = default(json.toString)
  }

  case object DateTimeIso8601JavaInstant extends DateTimeIso8601 {
    override def namespace: Option[String] = Some("_root_.java.time")
    def apiBuilderType = "date-time-iso8601"
    def shortName = "Instant"
    override def fromStringValue(value: String) = s"_root_.java.time.OffsetDateTime.parse(${ScalaUtil.quoteNameIfKeyword(value)}).toInstant"
    override def default(value: String): String = fromStringValue(ScalaUtil.wrapInQuotes(value))
  }

  case object DateTimeIso8601JavaOffsetDateTime extends DateTimeIso8601 {
    override def namespace: Option[String] = Some("_root_.java.time")
    def apiBuilderType = "date-time-iso8601"
    def shortName = "OffsetDateTime"
    override def fromStringValue(value: String) = s"_root_.java.time.OffsetDateTime.parse(${ScalaUtil.quoteNameIfKeyword(value)})"
    override def default(value: String): String = fromStringValue(ScalaUtil.wrapInQuotes(value))
  }

  case object Decimal extends ScalaPrimitive {
    def apiBuilderType = "decimal"
    def shortName = "BigDecimal"
    override protected def default(json: JsValue): String = json.as[scala.BigDecimal].toString
  }

  sealed trait JsonObject extends ScalaPrimitive

  case object ObjectAsPlay extends JsonObject {
    override def namespace: Option[String] = Some("_root_.play.api.libs.json")
    def apiBuilderType = "object"
    def shortName = "JsObject"
  }

  case object ObjectAsCirce extends JsonObject {
    override def namespace: Option[String] = None
    def apiBuilderType = "object"
    def shortName = "Map[String, _root_.io.circe.Json]"
    override def asString(originalVarName: String): String = s"${ScalaUtil.quoteNameIfKeyword(originalVarName)}.asJson"
  }

  sealed trait JsonValue extends ScalaPrimitive

  case object JsonValueAsPlay extends JsonValue {
    override def namespace: Option[String] = Some("_root_.play.api.libs.json")
    def apiBuilderType = "json"
    def shortName = "JsValue"
  }

  case object JsonValueAsCirce extends JsonValue {
    override def namespace: Option[String] = Some("_root_.io.circe")
    def apiBuilderType = "json"
    def shortName = "Json"
    override def asString(originalVarName: String): String = s"${ScalaUtil.quoteNameIfKeyword(originalVarName)}.asJson"
  }

  case object String extends ScalaPrimitive {
    def apiBuilderType = "string"
    def shortName = "String"
    override def asString(originalVarName: String): String = ScalaUtil.quoteNameIfKeyword(originalVarName)
    override def fromStringValue(value: String): String = ScalaUtil.quoteNameIfKeyword(value)
    override def default(value: String): String = ScalaUtil.wrapInQuotes(value)
    override protected def default(json: JsValue): String = default(json.as[String])
  }

  case object Unit extends ScalaPrimitive {
    def apiBuilderType = "unit"
    def shortName = "Unit"
    override def asString(originalVarName: String): String = {
      throw new UnsupportedOperationException(s"unsupported conversion of type object for $originalVarName")
    }
  }

  case object Uuid extends ScalaPrimitive {
    override def namespace: Option[String] = Some("_root_.java.util")
    def apiBuilderType = "uuid"
    def shortName = "UUID"
    override def fromStringValue(value: String) = s"_root_.java.util.UUID.fromString(${ScalaUtil.quoteNameIfKeyword(value)})"
    override def default(value: String): String = fromStringValue(ScalaUtil.wrapInQuotes(value))

    override protected def default(json: JsValue): String = default(json.as[UUID].toString)

  }

  case class Model(namespaces: Namespaces, shortName: String) extends ScalaPrimitive {
    override def namespace: Option[String] = Some(namespaces.models)
    def apiBuilderType: String = shortName
    override def toVariableName: String = initLowerCase(shortName)
  }

  case class GeneratedModel(shortName: String) extends ScalaPrimitive {
    override def namespace: Option[String] = None
    def apiBuilderType: String = shortName
    override def toVariableName: String = initLowerCase(shortName)
  }

  case class Enum(namespaces: Namespaces, shortName: String) extends ScalaPrimitive {
    override def namespace: Option[String] = Some(namespaces.enums)
    def apiBuilderType: String = shortName
    override def default(value: String): String = fullName + "." + ScalaUtil.toClassName(value)
    override protected def default(json: JsValue): String = default(json.as[String])
    override def toVariableName: String = initLowerCase(shortName)
  }

  case class Union(namespaces: Namespaces, shortName: String) extends ScalaPrimitive {
    override def namespace: Option[String] = Some(namespaces.unions)
    def apiBuilderType: String = shortName
    override def toVariableName: String = initLowerCase(shortName)
  }

}

object ScalaDatatype {
  sealed abstract class Container(inner: ScalaDatatype) extends ScalaDatatype {
    override def toVariableName: String = inner match {
      case _: Container => inner.toVariableName
      case _ => lib.Text.pluralize(inner.toVariableName)
    }
  }

  case class List(inner: ScalaDatatype) extends Container(inner) {
    override def name = s"Seq[${inner.name}]"

    override protected def default(json: JsValue): String = {
      val arr = json.as[JsArray]
      val seq = arr.value.map { value =>
        inner.default(value)
      }
      if (seq.isEmpty) "Nil" else seq.mkString(s"scala.List(", ",", ")")
    }
  }

  case class Map(inner: ScalaDatatype) extends Container(inner) {
    override def name = s"Map[String, ${inner.name}]"

    override protected def default(json: JsValue): String = {
      val map = json.as[scala.collection.immutable.Map[String, JsValue]].map {
        case (key, value) => s""""${key}" -> ${inner.default(value)}"""
      }
      if (map.isEmpty) "Map.empty" else map.mkString(s"Map(", ",", ")")
    }
  }

  case class Option(datatype: ScalaDatatype) extends Container(datatype) {
    override def name = s"_root_.scala.Option[${datatype.name}]"

    override def definition(
      originalVarName: String,
      default: scala.Option[String],
      deprecation: scala.Option[Deprecation],
      scalaTypeKind: ScalaTypeKind,
    ): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"${deprecationString(deprecation)}${appendSpace(scalaTypeKind.toString)}$varName: $name = ${default.getOrElse("None")}"
    }

    // override, since options contain at most one element
    override def toVariableName: String = datatype.toVariableName
  }

}

object ScalaTypeResolver {

  /**
   * If name is a qualified name (as identified by having a dot),
   * parses the name and returns a tuple of (namespace,
   * name). Otherwise, returns (Namespaces object, class name)
   */
  private def parseQualifiedName(defaultNamespaces: Namespaces, name: String): (Namespaces, String) = {
    name.split("\\.").toList match {
      case _ :: Nil => (defaultNamespaces, ScalaUtil.toClassName(name))
      case multiple =>
        val n = multiple.last
        val baseNamespace = multiple.reverse.drop(2).reverse.mkString(".")
        (Namespaces(baseNamespace), ScalaUtil.toClassName(n))
    }
  }

}

case class ScalaTypeResolver(
  namespaces: Namespaces
) {

  def scalaDatatype(datatype: Datatype): ScalaDatatype = {
    datatype match {
      case Datatype.Primitive.Boolean => ScalaPrimitive.Boolean
      case Datatype.Primitive.Decimal => ScalaPrimitive.Decimal
      case Datatype.Primitive.Integer => ScalaPrimitive.Integer
      case Datatype.Primitive.Double => ScalaPrimitive.Double
      case Datatype.Primitive.Long => ScalaPrimitive.Long
      case Datatype.Primitive.Object => ScalaPrimitive.ObjectAsPlay
      case Datatype.Primitive.JsonValue => ScalaPrimitive.JsonValueAsPlay
      case Datatype.Primitive.String => ScalaPrimitive.String
      case Datatype.Primitive.DateIso8601 => ScalaPrimitive.DateIso8601Joda
      case Datatype.Primitive.DateTimeIso8601 => ScalaPrimitive.DateTimeIso8601Joda
      case Datatype.Primitive.Uuid => ScalaPrimitive.Uuid
      case Datatype.Primitive.Unit => ScalaPrimitive.Unit

      case Datatype.Container.List(t) => ScalaDatatype.List(scalaDatatype(t))
      case Datatype.Container.Map(t) => ScalaDatatype.Map(scalaDatatype(t))
      case Datatype.Container.Option(inner) => ScalaDatatype.Option(scalaDatatype(inner))

      case Datatype.Generated.Model(name) => ScalaPrimitive.GeneratedModel(name)
      case Datatype.UserDefined.Model(name) => {
        name.split("\\.").toList match {
          case n :: Nil => ScalaPrimitive.Model(namespaces, ScalaUtil.toClassName(n))
          case _ => {
            val (ns, n) = ScalaTypeResolver.parseQualifiedName(namespaces, name)
            ScalaPrimitive.Model(ns, n)
          }
        }
      }
      case Datatype.UserDefined.Enum(name) => {
        val (ns, n) = ScalaTypeResolver.parseQualifiedName(namespaces, name)
        ScalaPrimitive.Enum(ns, n)
      }
      case Datatype.UserDefined.Union(name) => {
        val (ns, n) = ScalaTypeResolver.parseQualifiedName(namespaces, name)
        ScalaPrimitive.Union(ns, n)
      }
    }
  }

}

sealed trait ScalaTypeKind

object ScalaTypeKind {
  case object DefKind extends ScalaTypeKind { override val toString = "def" }
  case object CaseClassFieldKind extends ScalaTypeKind { override val toString = "" }
  case object OverrideValKind extends ScalaTypeKind { override val toString = "override val" }
  case object ParameterKind extends ScalaTypeKind { override val toString = "" }
}
