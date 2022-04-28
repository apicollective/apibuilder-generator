package scala.generator

sealed trait DiscriminatorValue {
  def generateCode: String
}

object DiscriminatorValue {
  private[this] def declaration(discriminatorField: ScalaField): String = {
    s"override val ${ScalaUtil.quoteNameIfKeyword(discriminatorField.field.name)}: ${discriminatorField.field.`type`} = ${discriminatorField.field.`type`}."
  }

  case class TypeModel(discriminatorField: ScalaField, unionType: ScalaUnionType) extends DiscriminatorValue {
    override def generateCode: String = {
      s"${declaration(discriminatorField)}${unionType.name}"
    }
  }
  case class Undefined(discriminatorField: ScalaField, wrapper: UnionTypeUndefinedModelWrapper) extends DiscriminatorValue {
    override def generateCode: String = {
      s"${declaration(discriminatorField)}UNDEFINED(${wrapper.descriptionField.name})"
    }
  }

  def generateCode(`enum`: ScalaEnum, unions: Seq[ScalaUnion]): Seq[String] = {
    unions.flatMap { u =>
      generate(u, enum.name)
    }.distinct.map(_.generateCode)
  }

  def generateCode(model: ScalaModel, unions: Seq[ScalaUnion]): Seq[String] = {
    unions.flatMap { u =>
      generate(u, model.name)
    }.distinct.map(_.generateCode)
  }

  private[this] def generate(union: ScalaUnion, typeName: String): Option[DiscriminatorValue] = {
    union.discriminatorField.flatMap { d =>
      union.types.find(_.name == typeName) match {
        case Some(t) => Some(DiscriminatorValue.TypeModel(d.field, t))
        case None if typeName == union.undefinedType.model.name => Some(
          DiscriminatorValue.Undefined(d.field, union.undefinedType)
        )
        case None => None
      }
    }
  }
}
