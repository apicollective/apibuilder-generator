package generator

case class PrimitiveWrapper(ssd: ScalaService) {

  // case class UuidWrapper(value: _root_.java.util.UUID) extends User

  def generate(): Option[String] = {
    ssd.unions.flatMap(_.types).map(_.datatype.primitive).flatMap(generate(_)) match {
      case Nil => None
      case fragments => Some(fragments.mkString("\n\n"))
    }
  }

  private def generate(primitive: ScalaPrimitive): Option[String] = {
    val unions = ssd.unions.filter { union =>
      union.types.map(_.datatype.primitive).contains(primitive)
    }
    assert(!unions.isEmpty, s"Primitive[${primitive.shortName}] is not used by a union type")

    primitive match {
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        None
      }
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid => {

        val extendsClause = ScalaUtil.extendsClause(unions.map(_.name)).getOrElse("")
        Some(s"case class ${primitive.shortName}Wrapper(value: ${primitive.fullName}) $extendsClause")
      }
    }
  }

}
