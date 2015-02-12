package generator

import com.gilt.apidoc.spec.v0.models.{Field, Model}

case class UnionTypeUndefinedModel(ssd: ScalaService) {

  case class Wrapper(model: ScalaModel, union: ScalaUnion)

  val models: Seq[Wrapper] = ssd.unions.map { union =>
    // TODO: Verify another type does not exist with this name
    val name = s"${union.name}UndefinedType"
    val model = Model(
      name = name,
      plural = s"${name}s",
      description = Some(s"Provides future compatibility in clients - in the future, when a type is added to the union ${union.name}, it will need to be handled in the client code. This implementation will deserialize these future types as an instance of this class."),
      fields = Seq(
        Field(
          name = "description",
          description = Some(s"Information about the type that we received that is undefined in this version of the client."),
          `type` = lib.Primitives.String.toString,
          required = true
        )
      )
    )
    Wrapper(
      new ScalaModel(ssd, model),
      union
    )
  }

}
