package scala.generator

import io.apibuilder.spec.v0.models.{Field, Model}

object PrimitiveWrapper {

  val FieldName: String = "value"

  def className(union: ScalaUnion, primitive: ScalaPrimitive): String = {
    primitive match {
      case _ @ (ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | _: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | _: ScalaPrimitive.JsonObject | _: ScalaPrimitive.JsonValue | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid) => {
        ScalaUtil.toClassName(union.name) + ScalaUtil.toClassName(primitive.shortName)
      }
      case _: ScalaPrimitive.GeneratedModel | _: ScalaPrimitive.Model | _: ScalaPrimitive.Enum | _: ScalaPrimitive.Union => {
        ScalaUtil.toClassName(union.name)
      }
    }
  }

  def isBasicType(primitive: ScalaPrimitive): Boolean = {
    primitive match {
      case _: ScalaPrimitive.GeneratedModel | _: ScalaPrimitive.Model | _: ScalaPrimitive.Enum | _: ScalaPrimitive.Union => {
        false
      }
      case _ @ (ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | _: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | _: ScalaPrimitive.JsonObject | _: ScalaPrimitive.JsonValue | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid) => {
        true
      }
    }
  }

  def needsWrapper(datatype: ScalaDatatype): Boolean = {
    datatype match {
      case p: ScalaPrimitive if PrimitiveWrapper.isBasicType(p) => true
      case _ => false
    }
  }
}

case class PrimitiveWrapper(ssd: ScalaService) {

  case class Wrapper(model: ScalaModel, union: ScalaUnion)

  val wrappers: Seq[Wrapper] = ssd.unions.flatMap { union =>
    union.types.flatMap { t =>
      t.datatype match {
        case p: ScalaPrimitive if PrimitiveWrapper.isBasicType(p) => {
          val name = PrimitiveWrapper.className(union, p)
          val model = Model(
            name = name,
            plural = s"${name}s",
            description = Some(s"Wrapper class to support the union types containing the datatype[${p.apiBuilderType}]"),
            fields = Seq(
              Field(
                name = PrimitiveWrapper.FieldName,
                `type` = p.apiBuilderType,
                required = true
              )
            )
          )
          Some(
            Wrapper(
              new ScalaModel(ssd, model),
              union,
            )
          )
        }
        case _ => None
      }
    }
  }

}
