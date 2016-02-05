package go.models

import lib.Datatype

case class GoType(className: String, datatype: Datatype)

object GoType {

  case class RubyTypeInfo(
    varName: String,
    klass: String,
    assertMethod: String,
    datatype: Datatype
  )

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

  def apply(
    dt: Datatype
  ): GoType = {
    GoType(
      className = className(dt),
      datatype = dt
    )
  }

}
