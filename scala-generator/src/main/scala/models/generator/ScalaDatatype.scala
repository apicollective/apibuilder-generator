package scala.generator

import java.util.UUID
import java.time.LocalDate
import lib.Datatype
import lib.generator.GeneratorUtil
import lib.Text.initLowerCase
import play.api.libs.json._
import play.api.Logger

import com.bryzek.apidoc.spec.v0.models.Deprecation
import scala.util.{Failure, Success, Try}

sealed trait ScalaDatatype {
  def asString(originalVarName: String): String = {
    throw new UnsupportedOperationException(s"unsupported conversion of type ${name} for var $originalVarName")
  }

  def name: String

  def deprecationString(deprecation: Option[Deprecation]): String =
    ScalaUtil.deprecationString(deprecation)

  def definition(
    originalVarName: String,
    default: Option[String],
    deprecation: Option[Deprecation]
  ): String = {
    val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
    default.fold(s"${deprecationString(deprecation)}$varName: $name") { default =>
      s"$varName: $name = $default"
    }
  }

  def default(value: String): String = {
    Try {
      Json.parse(value)
    } match {
      case Success(js) => default(js)
      case Failure(ex) => default(JsString(value))
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
  def apidocType: String
  def shortName: String
  override def asString(originalVarName: String): String
  def namespace: Option[String] = None
  def fullName: String = namespace match {
    case None => shortName
    case Some(ns) => s"$ns.$shortName"
  }
  def name = fullName

  override def toVariableName = "value"
}

object ScalaPrimitive {

  case object Boolean extends ScalaPrimitive {
    def apidocType = "boolean"
    def shortName = "Boolean"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = json.as[scala.Boolean].toString
  }

  case object Double extends ScalaPrimitive {
    def apidocType = "double"
    def shortName = "Double"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = toBigDecimal(json).toDouble.toString
  }

  case object Integer extends ScalaPrimitive {
    def apidocType = "integer"
    def shortName = "Int"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = toBigDecimal(json).toInt.toString
  }

  case object Long extends ScalaPrimitive {
    def apidocType = "long"
    def shortName = "Long"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = toBigDecimal(json).toLong.toString
  }

  case object DateIso8601 extends ScalaPrimitive {
    override def namespace = Some("_root_.java.time")
    def apidocType = "date-iso8601"
    def shortName = "LocalDate"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def default(value: String) = default(JsString(value))


    override protected def default(json: JsValue) = {
      val dt = LocalDate.parse(json.as[String])
      s"${fullName}.of(${dt.getYear}, ${dt.getMonthValue}, ${dt.getDayOfMonth})"
    }
  }

  case object DateTimeIso8601 extends ScalaPrimitive {
    override def namespace = Some("_root_.java.time")
    def apidocType = "date-time-iso8601"
    def shortName = "DateTime"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"_root_.java.time.format.DateTimeFormatter.ISO_DATE_TIME.format($varName)"
    }

    override def default(value: String) = {
      "_root_.java.time.ZonedDateTime.parse(" + ScalaUtil.wrapInQuotes(value) + ")"
    }

    override protected def default(json: JsValue) = {
      // TODO would like to use the constructor for DateTime, since that would
      // be faster code, but things get quite tricky because of time zones :(
      s"""_root_.java.time.ZonedDateTime.parse(${json})"""
    }
  }

  case object Decimal extends ScalaPrimitive {
    def apidocType = "decimal"
    def shortName = "BigDecimal"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = json.as[scala.BigDecimal].toString
  }

  case object Object extends ScalaPrimitive {
    override def namespace = Some("_root_.play.api.libs.json")
    def apidocType = "object"
    def shortName = "JsObject"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object String extends ScalaPrimitive {
    def apidocType = "string"
    def shortName = "String"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName"
    }

    override def default(value: String) = ScalaUtil.wrapInQuotes(value)

    override protected def default(json: JsValue) = default(json.as[String])
  }

  case object Unit extends ScalaPrimitive {
    def apidocType = "unit"
    def shortName = "Unit"
    override def asString(originalVarName: String): String = {
      throw new UnsupportedOperationException(s"unsupported conversion of type object for $originalVarName")
    }
  }

  case object Uuid extends ScalaPrimitive {
    override def namespace = Some("_root_.java.util")
    def apidocType = "uuid"
    def shortName = "UUID"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def default(value: String) = "_root_.java.util.UUID.fromString(" + ScalaUtil.wrapInQuotes(value) + ")"

    override protected def default(json: JsValue) = default(json.as[UUID].toString)

  }

  case class Model(namespaces: Namespaces, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(namespaces.models)
    def apidocType = shortName
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def toVariableName = initLowerCase(shortName)
  }

  case class Enum(namespaces: Namespaces, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(namespaces.enums)
    def apidocType = shortName
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def default(value: String): String = {
      fullName + "." + ScalaUtil.toClassName(value)
    }

    override protected def default(json: JsValue) = {
      default(json.as[String])
    }

    override def toVariableName = initLowerCase(shortName)
  }

  case class Union(namespaces: Namespaces, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(namespaces.unions)
    def apidocType = shortName
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def toVariableName = initLowerCase(shortName)
  }

}

object ScalaDatatype {
  sealed abstract class Container(inner: ScalaDatatype) extends ScalaDatatype {
    override def toVariableName = inner match {
      case _: Container => inner.toVariableName
      case _ => lib.Text.pluralize(inner.toVariableName)
    }
  }

  case class List(inner: ScalaDatatype) extends Container(inner) {
    override def name = s"Seq[${inner.name}]"

    override protected def default(json: JsValue) = {
      val arr = json.as[JsArray]
      val seq = arr.value.map { value =>
        inner.default(value)
      }
      if (seq.isEmpty) "Nil" else seq.mkString(s"Seq(", ",", ")")
    }
  }

  case class Map(inner: ScalaDatatype) extends Container(inner) {
    override def name = s"Map[String, ${inner.name}]"

    override protected def default(json: JsValue) = {
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
      deprecation: scala.Option[Deprecation]
    ): String = {
      require(default.isEmpty, s"no defaults allowed on options: ${default}")
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"${deprecationString(deprecation)}$varName: $name = None"
    }

    // override, since options contain at most one element
    override def toVariableName = datatype.toVariableName
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
      case n :: Nil => (defaultNamespaces, ScalaUtil.toClassName(name))
      case multiple =>
        val n = multiple.last
        val objectType = GeneratorUtil.ObjectType.fromString(multiple.reverse.drop(1).reverse.last).getOrElse {
          Logger.warn(s"Could not resolve object type[${multiple.reverse.drop(1).reverse.last}]. Defaults to models")
          GeneratorUtil.ObjectType.Model
        }
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
      case Datatype.Primitive.Object => ScalaPrimitive.Object
      case Datatype.Primitive.String => ScalaPrimitive.String
      case Datatype.Primitive.DateIso8601 => ScalaPrimitive.DateIso8601
      case Datatype.Primitive.DateTimeIso8601 => ScalaPrimitive.DateTimeIso8601
      case Datatype.Primitive.Uuid => ScalaPrimitive.Uuid
      case Datatype.Primitive.Unit => ScalaPrimitive.Unit

      case Datatype.Container.List(t) => ScalaDatatype.List(scalaDatatype(t))
      case Datatype.Container.Map(t) => ScalaDatatype.Map(scalaDatatype(t))
      case Datatype.Container.Option(inner) => ScalaDatatype.Option(scalaDatatype(inner))

      case Datatype.UserDefined.Model(name) => {
        name.split("\\.").toList match {
          case n :: Nil => ScalaPrimitive.Model(namespaces, ScalaUtil.toClassName(n))
          case multiple => {
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
