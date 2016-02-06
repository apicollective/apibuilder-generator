package go.models

import lib.{Datatype, Text}

case class GoType(className: String, datatype: Datatype) {

  /**
    * Based on the datatype, generates a variable name - e.g. if the
    * type is 'tag', returns tag. If the type is []tag, returns tags
    */
  def classVariableName(): String = {
    classVariableName(datatype)
  }

  private[this] def classVariableName(datatype: Datatype): String = {
    datatype match {
      case p: Datatype.Primitive => "value"
      case Datatype.UserDefined.Model(name) => GoUtil.privateName(name)
      case Datatype.UserDefined.Union(name) => GoUtil.privateName(name)
      case Datatype.UserDefined.Enum(name) => "value"
      case Datatype.Container.Option(inner) => classVariableName(inner)
      case Datatype.Container.Map(inner) => GoUtil.privateName(Text.pluralize(classVariableName(inner)))
      case Datatype.Container.List(inner) => GoUtil.privateName(Text.pluralize(classVariableName(inner)))
    }
  }

  /**
    * True if this dataytpe returns multiple values
    */
  def isMulti(): Boolean = {
    isMulti(datatype)
  }

  private[this] def isMulti(datatype: Datatype): Boolean = {
    datatype match {
      case p: Datatype.Primitive => false
      case Datatype.UserDefined.Model(name) => false
      case Datatype.UserDefined.Union(name) => false
      case Datatype.UserDefined.Enum(name) => false
      case Datatype.Container.Option(inner) => isMulti(inner)
      case Datatype.Container.Map(inner) => true
      case Datatype.Container.List(inner) => true
    }
  }

  def toEscapedString(varName: String): String = {
    val expr = toString(varName)
    expr == varName match {
      case true => s"html.EscapeString($varName)"
      case false => expr
    }
  }

  def toString(varName: String): String = {
    toString(varName, datatype)
  }

  private[this] def toString(varName: String, dt: Datatype): String = {
    dt match {
      case Datatype.Primitive.Boolean => s"strconv.FormatBool($varName)"
      case Datatype.Primitive.Double => s"strconv.FormatFloat($varName, 'E', -1, 64)"
      case Datatype.Primitive.Integer => s"strconv.iota($varName, 10)"
      case Datatype.Primitive.Long => s"strconv.iota($varName, 10)"
      case Datatype.Primitive.DateIso8601 => varName     // TODO
      case Datatype.Primitive.DateTimeIso8601 => varName // TODO
      case Datatype.Primitive.Decimal => varName
      case Datatype.Primitive.Object => sys.error("Object cannot be converted to escaped string")
      case Datatype.Primitive.String => varName
      case Datatype.Primitive.Unit => "nil"
      case Datatype.Primitive.Uuid => varName
      case u: Datatype.UserDefined => {
        u match {
          case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) => {
            sys.error("User defined type cannot be converted to escaped string")
          }
          case Datatype.UserDefined.Enum(name) => {
            varName
          }
        }
      }
      case Datatype.Container.Option(inner) => toString(varName, inner)
      case Datatype.Container.Map(_) | Datatype.Container.List(_) => {
        sys.error("Collections cannot be converted to escaped string")
      }
    }
  }

  def nil(varName: String): String = {
    compareToImplicitValue(varName, datatype, "==")
  }

  def notNil(varName: String): String = {
    compareToImplicitValue(varName, datatype, "!=")
  }

  private[this] def compareToImplicitValue(varName: String, dt: Datatype, operator: String): String = {
    dt match {
      case Datatype.Primitive.Boolean => "false"
      case Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long => {
        s"$varName $operator 0"
      }
      case Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid | Datatype.UserDefined.Enum(_) => {
        s"""$varName $operator """""
      }
      case Datatype.Primitive.Object | Datatype.Primitive.Unit | Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) | Datatype.Container.List(_) => {
        s"""$varName $operator nil"""""
      }
      case Datatype.Container.Option(inner) => {
        compareToImplicitValue(varName, inner, operator)
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
