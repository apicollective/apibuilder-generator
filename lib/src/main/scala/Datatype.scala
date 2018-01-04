package lib

import scala.util.Try
import scala.util.Success

sealed abstract class Datatype(val name: String)

object Datatype {
  sealed abstract class Primitive(override val name: String)
    extends Datatype(name)

  object Primitive {
    def apply(name: String): Try[Primitive] = Try(name match {
      case "boolean" => Boolean
      case "double" => Double
      case "integer" => Integer
      case "long" => Long
      case "date-iso8601" => DateIso8601
      case "date-time-iso8601" => DateTimeIso8601
      case "decimal" => Decimal
      case "object" => Object
      case "json" => JsonValue
      case "string" => String
      case "unit" => Unit
      case "uuid" => Uuid
    })

    case object Boolean extends Primitive("boolean")
    case object Double extends Primitive("double")
    case object Integer extends Primitive("integer")
    case object Long extends Primitive("long")
    case object DateIso8601 extends Primitive("date-iso8601")
    case object DateTimeIso8601 extends Primitive("date-time-iso8601")
    case object Decimal extends Primitive("decimal")
    case object Object extends Primitive("object")
    case object JsonValue extends Primitive("json")
    case object String extends Primitive("string")
    case object Unit extends Primitive("unit")
    case object Uuid extends Primitive("uuid")
  }

  sealed abstract class UserDefined(override val name: String)
    extends Datatype(name)

  object UserDefined {
    case class Model(override val name: String) extends UserDefined(name)

    case class Enum(override val name: String) extends UserDefined(name)

    case class Union(override val name: String) extends UserDefined(name)
  }

  sealed abstract class Container(val inner: Datatype, prefix: String, suffix: String)
    extends Datatype(s"${prefix}${inner.name}${suffix}")

  object Container {
    case class List(override val inner: Datatype) extends Container(inner, "[", "]")

    case class Map(override val inner: Datatype) extends Container(inner, "map[", "]")

    case class Option(override val inner: Datatype) extends Container(inner, "option[", "]")
  }
}

case class DatatypeResolver(
  enumNames: Iterable[String],
  unionNames: Iterable[String],
  modelNames: Iterable[String]
) {

  private[this] val userDefinedTypes = {
    val builder = Map.newBuilder[String, Datatype.UserDefined]
    enumNames.foreach { name =>
      builder += name -> Datatype.UserDefined.Enum(name)
    }
    unionNames.foreach { name =>
      builder += name -> Datatype.UserDefined.Union(name)
    }
    modelNames.foreach { name =>
      builder += name -> Datatype.UserDefined.Model(name)
    }
    builder.result
  }

  private object UserDefined {
    def unapply(value: String): Option[Datatype.UserDefined] = {
      userDefinedTypes.get(value)
    }
  }

  private final val ListRx = "^\\[(.*)\\]$".r
  private final val MapRx = "^map\\[(.*)\\]$".r
  private final val MapDefaultRx = "^map$".r

  /**
    * Parses a type string into an instance of a Datatype.
    * 
    * @param value: Examples: "string", "uuid"
    */
  def parse(value: String, required: Boolean): Try[Datatype] = {
    val dt = value match {
      case "boolean" => Success(Datatype.Primitive.Boolean)
      case "double" => Success(Datatype.Primitive.Double)
      case "integer" => Success(Datatype.Primitive.Integer)
      case "long" => Success(Datatype.Primitive.Long)
      case "date-iso8601" => Success(Datatype.Primitive.DateIso8601)
      case "date-time-iso8601" => Success(Datatype.Primitive.DateTimeIso8601)
      case "decimal" => Success(Datatype.Primitive.Decimal)
      case "object" => Success(Datatype.Primitive.Object)
      case "json" => Success(Datatype.Primitive.JsonValue)
      case "string" => Success(Datatype.Primitive.String)
      case "unit" => Success(Datatype.Primitive.Unit)
      case "uuid" => Success(Datatype.Primitive.Uuid)

      case ListRx(inner) => parse(inner, true).map(Datatype.Container.List)
      case MapRx(inner) => parse(inner, true).map(Datatype.Container.Map)
      case MapDefaultRx() => Success(Datatype.Container.Map(Datatype.Primitive.String))

      case UserDefined(dt) => Success(dt)
    }

    if (required) dt else dt.map(Datatype.Container.Option)
  }

}
