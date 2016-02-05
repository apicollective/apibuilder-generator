package go.models

import lib.Datatype

case class GoType(className: String, datatype: Datatype) {

  def toEscapedString(varName: String): String = {
    toEscapedString(varName, datatype)
    }

  private[this] def toEscapedString(varName: String, dt: Datatype): String = {
    dt match {
      case Datatype.Primitive.Boolean => s"strconv.FormatBool($varName)"
      case Datatype.Primitive.Double => s"strconv.FormatFloat($varName, 'E', -1, 64)"
      case Datatype.Primitive.Integer => s"strconv.iota($varName, 10)"
      case Datatype.Primitive.Long => s"strconv.iota($varName, 10)"
      case Datatype.Primitive.DateIso8601 => s"html.EscapeString($varName)"     // TODO
      case Datatype.Primitive.DateTimeIso8601 => s"html.EscapeString($varName)" // TODO
      case Datatype.Primitive.Decimal => s"html.EscapeString($varName)"
      case Datatype.Primitive.Object => sys.error("Object cannot be converted to escaped string")
      case Datatype.Primitive.String => s"html.EscapeString($varName)"
      case Datatype.Primitive.Unit => "nil"
      case Datatype.Primitive.Uuid => s"html.EscapeString($varName)"
      case u: Datatype.UserDefined => {
        u match {
          case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) => {
            sys.error("User defined type cannot be converted to escaped string")
          }
          case Datatype.UserDefined.Enum(name) => {
            s"html.EscapeString($varName)"
          }
        }
      }
      case Datatype.Container.Option(inner) => toEscapedString(varName, inner)
      case Datatype.Container.Map(_) | Datatype.Container.List(_) => {
        sys.error("Collections cannot be converted to escaped string")
      }
    }
  }

}

object GoType {

  def apply(
    dt: Datatype
  ): GoType = {
    GoType(
      className = className(dt),
      datatype = dt
    )
  }

  private[this] def className(dt: Datatype): String = {
    dt match {
      case Datatype.Primitive.Boolean => "bool"
      case Datatype.Primitive.Double => "float64"
      case Datatype.Primitive.Integer => "int32"
      case Datatype.Primitive.Long => "int64"
      case Datatype.Primitive.DateIso8601 => "string"
      case Datatype.Primitive.DateTimeIso8601 => "string"
      case Datatype.Primitive.Decimal => "string"
      case Datatype.Primitive.Object => "interface{}"
      case Datatype.Primitive.String => "string"
      case Datatype.Primitive.Unit => "nil"
      case Datatype.Primitive.Uuid => "string"
      case u: Datatype.UserDefined => u.name
      case Datatype.Container.Option(inner) => className(inner)
      case Datatype.Container.Map(inner) => "map[string]" + className(inner)
      case Datatype.Container.List(inner) => "[]" + className(inner)
    }
  }

}
