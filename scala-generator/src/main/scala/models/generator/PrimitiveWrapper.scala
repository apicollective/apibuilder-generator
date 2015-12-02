package scala.generator

import lib.Text

import com.bryzek.apidoc.spec.v0.models.{Field, Model}

object PrimitiveWrapper {

  def className(union: ScalaUnion, primitive: ScalaPrimitive): String = {
    primitive match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid => {
        ScalaUtil.toClassName(union.name) + ScalaUtil.toClassName(primitive.shortName)
      }
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        ScalaUtil.toClassName(union.name)
      }
    }
  }
}

case class PrimitiveWrapper(ssd: ScalaService) {

  case class Wrapper(model: ScalaModel, union: ScalaUnion)

  val wrappers: Seq[Wrapper] = ssd.unions.flatMap { union =>
    union.types.map(_.datatype).collect {
      case p: ScalaPrimitive => p
    }.sortWith(_.shortName < _.shortName).flatMap { p =>
      p match {
        case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) => {
          None
        }
        case ScalaPrimitive.Enum(_, _) => {
          Some(ScalaPrimitive.String)
        }
        case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid => {
          Some(p)
        }
      }
    }.map { wrapperType =>
      TypeInfo(PrimitiveWrapper.className(union, wrapperType), wrapperType.apidocType)
    }.distinct.sorted.map { typeinfo =>
      new Wrapper(
        new ScalaModel(ssd, typeinfo.model),
        union
      )
    }
  }

  private[this] case class TypeInfo(
    name: String,
    `type`: String
  ) extends Ordered[TypeInfo] {

    val model = Model(
      name = name,
      plural = Text.pluralize(name),
      description = Some(s"Wrapper class to support the union types containing the datatype[${`type`}]"),
      fields = Seq(
        Field(
          name = "value",
          `type` = `type`,
          required = true
        )
      )
    )

    def compare(that: TypeInfo) = name.toLowerCase.compare(that.name.toLowerCase)

  }

}
