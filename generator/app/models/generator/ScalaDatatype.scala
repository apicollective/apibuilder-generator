package generator

import java.util.UUID
import org.joda.time.format.ISODateTimeFormat.dateTimeParser

import lib.{Datatype, Primitives, Type, Kind}
import play.api.libs.json._

sealed trait ScalaDatatype {
  def asString(originalVarName: String): String = {
    throw new UnsupportedOperationException(s"unsupported conversion of type ${name} for var $originalVarName")
  }

  def name: String

  def definition(
    originalVarName: String,
    default: Option[String]
  ): String = {
    val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
    default.fold(s"$varName: $name") { default =>
      s"$varName: $name = $default"
    }
  }

  def default(value: String): String = default(Json.parse(value))

  protected def default(json: JsValue): String = {
    throw new UnsupportedOperationException(s"default for type ${name}")
  }
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

    override protected def default(json: JsValue) = json.as[scala.Double].toString
  }

  case object Integer extends ScalaPrimitive {
    def apidocType = "integer"
    def shortName = "Int"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = json.as[scala.Int].toString
  }

  case object Long extends ScalaPrimitive {
    def apidocType = "long"
    def shortName = "Long"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override protected def default(json: JsValue) = json.as[scala.Long].toString
  }

  case object DateIso8601 extends ScalaPrimitive {
    override def namespace = Some("_root_.org.joda.time")
    def apidocType = "date-iso8601"
    def shortName = "LocalDate"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def default(value: String) = default(JsString(value))


    override protected def default(json: JsValue) = {
      val dt = dateTimeParser.parseLocalDate(json.as[String])
      s"new ${fullName}(${dt.getYear}, ${dt.getMonthOfYear}, ${dt.getDayOfMonth})"
    }
  }

  case object DateTimeIso8601 extends ScalaPrimitive {
    override def namespace = Some("_root_.org.joda.time")
    def apidocType = "date-time-iso8601"
    def shortName = "DateTime"
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($varName)"
    }

    override def default(value: String) = default(JsString(value))

    override protected def default(json: JsValue) = {
      val dt = dateTimeParser.parseDateTime(json.as[String])
      // TODO would like to use the constructor for DateTime, since that would
      // be faster code, but things get quite tricky because of time zones :(
      s"""_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(${json})"""
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

    override def default(value: String) = default(JsString(value))

    override protected def default(json: JsValue) = s""""${json.as[String]}""""
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

    override def default(value: String) = default(JsString(value))

    override protected def default(json: JsValue) = {
      val uuid = json.as[UUID]
      s"new UUID(${uuid.getMostSignificantBits}, ${uuid.getLeastSignificantBits})"
    }
  }

  case class Model(ns: String, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(ns)
    def apidocType = shortName
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case class Enum(ns: String, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(ns)
    def apidocType = shortName
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }

    override def default(value: String): String = default(JsString(value))

    override protected def default(json: JsValue) = s"${fullName}(${json})"
  }

  case class Union(ns: String, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(ns)
    def apidocType = shortName
    override def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

}

object ScalaDatatype {
  sealed abstract class Container(inner: ScalaDatatype) extends ScalaDatatype

  case class List(primitive: ScalaPrimitive) extends Container(primitive) {
    override def name = s"Seq[${primitive.fullName}]"

    override protected def default(json: JsValue) = {
      val arr = json.as[JsArray]
      val seq = arr.value.map { value =>
        primitive.default(value)
      }
      if (seq.isEmpty) "Nil" else seq.mkString(s"Seq(", ",", ")")
    }
  }

  case class Map(primitive: ScalaPrimitive) extends Container(primitive) {
    override def name = s"Map[String, ${primitive.fullName}]"

    override protected def default(json: JsValue) = {
      val map = json.as[scala.collection.immutable.Map[String, JsValue]].map {
        case (key, value) => s""""${key}" -> ${primitive.default(value)}"""
      }
      if (map.isEmpty) "Map.empty" else map.mkString(s"Map(", ",", ")")
    }
  }

  case class Option(datatype: ScalaDatatype) extends Container(datatype) {
    override def name = s"_root_.scala.Option[${datatype.name}]"

    override def definition(
      originalVarName: String,
      default: scala.Option[String]
    ): String = {
      require(default.isEmpty, s"no defaults allowed on options: ${default}")
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName: $name = None"
    }
  }

}

case class ScalaTypeResolver(
  namespaces: Namespaces
) {

  def scalaPrimitive(t: Type): ScalaPrimitive = {
    t match {
      case Type(Kind.Primitive, name) => {
        Primitives(name).getOrElse {
          sys.error(s"Invalid primitive[$name]")
        } match {
          case Primitives.Boolean => ScalaPrimitive.Boolean
          case Primitives.Decimal => ScalaPrimitive.Decimal
          case Primitives.Integer => ScalaPrimitive.Integer
          case Primitives.Double => ScalaPrimitive.Double
          case Primitives.Long => ScalaPrimitive.Long
          case Primitives.Object => ScalaPrimitive.Object
          case Primitives.String => ScalaPrimitive.String
          case Primitives.DateIso8601 => ScalaPrimitive.DateIso8601
          case Primitives.DateTimeIso8601 => ScalaPrimitive.DateTimeIso8601
          case Primitives.Uuid => ScalaPrimitive.Uuid
          case Primitives.Unit => ScalaPrimitive.Unit
        }
      }
      case Type(Kind.Model, name) => {
        name.split("\\.").toList match {
          case n :: Nil => ScalaPrimitive.Model(namespaces.models, ScalaUtil.toClassName(n))
          case multiple => {
            val (ns, n) = parseQualifiedName(namespaces.models, name)
            ScalaPrimitive.Model(ns, n)
          }
        }
      }
      case Type(Kind.Enum, name) => {
        val (ns, n) = parseQualifiedName(namespaces.enums, name)
        ScalaPrimitive.Enum(ns, n)
      }
      case Type(Kind.Union, name) => {
        val (ns, n) = parseQualifiedName(namespaces.unions, name)
        ScalaPrimitive.Union(ns, n)
      }
    }
  }

  /**
   * If name is a qualified name (as identified by having a dot),
   * parses the name and returns a tuple of (namespace,
   * name). Otherwise, returns (defaultNamespace, name)
   */
  private def parseQualifiedName(defaultNamespace: String, name: String): (String, String) = {
    name.split("\\.").toList match {
      case n :: Nil => (defaultNamespace, ScalaUtil.toClassName(name))
      case multiple => 
        val n = multiple.last
        val ns = multiple.reverse.drop(1).reverse.mkString(".")
        (Namespaces.quote(ns), ScalaUtil.toClassName(n))
    }
  }

  def scalaDatatype(datatype: Datatype, required: Boolean): ScalaDatatype = {
    val base = datatype match {
      case Datatype.List(t) => ScalaDatatype.List(scalaPrimitive(t))
      case Datatype.Map(t) => ScalaDatatype.Map(scalaPrimitive(t))
      case Datatype.Singleton(t) => scalaPrimitive(t)
    }
    if (required) base else ScalaDatatype.Option(base)
  }

}
