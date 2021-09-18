package scala.generator

import lib.Datatype
import io.apibuilder.spec.v0.models.{Field, Model}

import scala.annotation.tailrec

case class UnionTypeUndefinedModelWrapper(model: ScalaModel, union: ScalaUnion, interfaceFields: Seq[ScalaField])

case class UnionTypeUndefinedModel(ssd: ScalaService) {

  private[this] def descriptionField(name: String): Field = Field(
    name = name,
    description = Some("Information about the type that we received that is undefined in this version of the client."),
    `type` = Datatype.Primitive.String.name,
    required = true,
  )

  val models: Seq[UnionTypeUndefinedModelWrapper] = ssd.unions.map { union =>
    // TODO: Verify another type does not exist with this name
    val interfaceFields = ssd.findAllInterfaceFields(union.union.interfaces)
    val name = s"${union.name}UndefinedType"
    val model = Model(
      name = name,
      plural = s"${name}s",
      description = Some(s"Provides future compatibility in clients - in the future, when a type is added to the union ${union.name}, it will need to be handled in the client code. This implementation will deserialize these future types as an instance of this class."),
      fields = buildFields(interfaceFields.map(_.name).toSet),
    )

    UnionTypeUndefinedModelWrapper(
      model = new ScalaModel(ssd, model),
      union = union,
      interfaceFields = interfaceFields,
    )
  }

  @tailrec
  private[this] def buildFields(interfaceFieldNames: Set[String], count: Int = 0): Seq[Field] = {
    val finalName = count match {
      case 0 => "description"
      case 1 => "typeDescription"
      case n => s"type${n}Description"
    }

    if (interfaceFieldNames.contains(finalName)) {
      buildFields(interfaceFieldNames, count = count + 1)
    } else {
      Seq(descriptionField(finalName))
    }
  }
}
