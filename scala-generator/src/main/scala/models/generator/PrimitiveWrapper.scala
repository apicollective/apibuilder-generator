package generator

import com.gilt.apidoc.spec.v0.models.{Field, Model}

object PrimitiveWrapper {

  def className(union: ScalaUnion, primitive: ScalaPrimitive): String = {
    ScalaUtil.toClassName(union.name) + ScalaUtil.toClassName(primitive.shortName)
  }

}

case class PrimitiveWrapper(ssd: ScalaService) {

  case class Wrapper(model: ScalaModel, union: ScalaUnion)

  private val primitives = ssd.unions.flatMap(_.types).map(_.datatype).collect {
    case p: ScalaPrimitive => p
  }.filter(isBasicType(_)).sortWith(_.shortName < _.shortName)

  val wrappers: Seq[Wrapper] = ssd.unions.flatMap { union =>
    union.types.map(_.datatype).collect {
      case p: ScalaPrimitive => p
    }.filter(isBasicType(_)).sortWith(_.shortName < _.shortName).map { p =>
      val name = PrimitiveWrapper.className(union, p)
      val model = Model(
        name = name,
        plural = s"${name}s",
        description = Some(s"Wrapper class to support the union types containing the datatype[${p.apidocType}]"),
        fields = Seq(
          Field(
            name = "value",
            `type` = p.apidocType,
            required = true
          )
        )
      )
      new Wrapper(
        new ScalaModel(ssd, model),
        union
      )
    }
  }

  private def isBasicType(primitive: ScalaPrimitive): Boolean = {
    primitive match {
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        false
      }
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid => {

        true
      }
    }
  }

}
