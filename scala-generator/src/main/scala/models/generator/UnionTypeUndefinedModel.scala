package scala.generator

import lib.Datatype
import io.apibuilder.spec.v0.models.{Field, Model}

import scala.annotation.tailrec

case class UnionTypeUndefinedModelWrapper(
  union: ScalaUnion,
  model: ScalaModel,
  interfaceFields: Seq[ScalaField],
  descriptionField: Field,
  datatype: ScalaPrimitive.Model,
)

object UnionTypeUndefinedModel {

  def build(ssd: ScalaService, union: ScalaUnion): UnionTypeUndefinedModelWrapper = {
    // TODO: Verify another type does not exist with this name
    val name = union.name + "UndefinedType"

    val interfaceFields = ssd.findAllInterfaceFields(union.union.interfaces)
    val descField = buildDescriptionField(interfaceFields.map(_.name).toSet)
    val model = Model(
      name = name,
      plural = s"${name}s",
      description = Some(s"Provides future compatibility in clients - in the future, when a type is added to the union ${union.name}, it will need to be handled in the client code. This implementation will deserialize these future types as an instance of this class."),
      fields = Seq(descField),
    )

    UnionTypeUndefinedModelWrapper(
      union = union,
      model = new ScalaModel(ssd, model),
      interfaceFields = interfaceFields,
      descriptionField = descField,
      datatype = ScalaPrimitive.Model(ssd.namespaces, name)
    )
  }

  @tailrec
  private[this] def buildDescriptionField(interfaceFieldNames: Set[String], count: Int = 0): Field = {
    val finalName = count match {
      case 0 => "description"
      case 1 => "typeDescription"
      case n => s"type${n}Description"
    }

    if (interfaceFieldNames.contains(finalName)) {
      buildDescriptionField(interfaceFieldNames, count = count + 1)
    } else {
      Field(
        name = finalName,
        description = Some("Information about the type that we received that is undefined in this version of the client."),
        `type` = Datatype.Primitive.String.name,
        required = true,
      )
    }
  }
}
