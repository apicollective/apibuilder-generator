package generator

import com.gilt.apidoc.spec.v0.models.{Field, Model}

object PrimitiveWrapper {

  def className(primitive: ScalaPrimitive): String = {
    s"${ScalaUtil.toClassName(primitive.shortName)}Wrapper"
  }

}

case class PrimitiveWrapper(ssd: ScalaService) {

  private val primitives = ssd.unions.flatMap(_.types).map(_.datatype.primitive).filter(isBasicType(_)).sortWith(_.shortName < _.shortName)

  val models: Seq[ScalaModel] = primitives.map { p =>
    val name = PrimitiveWrapper.className(p)
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
    new ScalaModel(ssd, model)
  }

  private def unions(primitive: ScalaPrimitive): Seq[ScalaUnion] = {
    ssd.unions.filter { union =>
      union.types.map(_.datatype.primitive).contains(primitive)
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
