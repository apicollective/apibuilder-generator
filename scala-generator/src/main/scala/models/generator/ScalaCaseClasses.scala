package scala.generator

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator

import scala.models.{ApiBuilderComments, Attributes}

object ScalaCaseClasses extends ScalaCaseClasses

trait ScalaCaseClasses extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = invoke(form, addHeader = true)

  def invoke(
    form: InvocationForm,
    addHeader: Boolean = true
  ): Either[Seq[String], Seq[File]] = {
    val ssd = new ScalaService(form.service, Attributes.PlayDefaultConfig.withAttributes(form.attributes))
    Right(generateCode(ssd, form.userAgent, addHeader))
  }

  def generateCode(
    ssd: ScalaService,
    userAgent: Option[String],
    addHeader: Boolean = true,
    additionalImports: Seq[String] = Seq.empty
  ): Seq[File] = {
    val header = if (addHeader) {
      ApiBuilderComments(ssd.service.version, userAgent).toJavaString + "\n"
    } else {
      ""
    }

    val undefinedModels = ssd.unions.map(_.undefinedType).toList match {
      case Nil => ""
      case models => {
        models.map(generateUnionTypeUndefined).mkString("\n\n").indentString(2)
      }
    }

    val wrappers = PrimitiveWrapper(ssd).wrappers match {
      case Nil => ""
      case primitives => {
        primitives.map { w => generateCaseClassWithDoc(w.model, Seq(w.union)) }.mkString("\n\n").indentString(2) + "\n"
      }
    }

    val generatedClasses = Seq(undefinedModels, wrappers).filter(_.nonEmpty) match {
      case Nil => ""
      case code => "\n" + code.mkString("\n\n")
    }

    val unionNames = ssd.unions.map(_.name)
    val source = s"${header}package ${ssd.namespaces.models} {\n\n  " +
    Seq(
      additionalImports.mkString("\n").indentString(2),
      ssd.interfaces
        .filterNot { i => unionNames.contains(i.name) }
        .map { i => generateTraitWithDoc(i) }.mkString("\n\n").indentString(2),
      ssd.unions.map { u => generateUnionTraitWithDocAndDiscriminator(ssd, u, ssd.unionsForUnion(u)) }.mkString("\n\n").indentString(2),
      "",
      ssd.models.map { m => generateCaseClassWithDoc(m, ssd.unionsForModel(m)) }.mkString("\n\n").indentString(2),
      generatedClasses,
      ssd.enums.map { generateEnum(ssd, _) }.mkString("\n\n").indentString(2)
    ).mkString("\n").trim +
    s"\n\n}"

    Seq(ServiceFileNames.toFile(ssd.service.namespace, ssd.service.organization.key, ssd.service.application.key, ssd.service.version, "Models", source, Some("Scala")))
  }

  def generateUnionTypeUndefined(wrapper: UnionTypeUndefinedModelWrapper): String = {
    val base = generateCaseClassWithDoc(wrapper.model, Seq(wrapper.union))
    val fields = wrapper.interfaceFields.map { f =>
      s"override def ${f.name}: ${f.datatype.name} = ???"
    }.mkString("\n")
    withInterfaceFields(base, fields)
  }

  private[this] def withInterfaceFields(base: String, fields: String): String = {
    if (fields.isEmpty) {
      base
    } else {
      Seq(
        s"$base {",
        fields.indentString(),
        "}",
      ).filterNot(_.isEmpty).mkString("\n")
    }
  }

  def generateUnionTraitWithDocAndDiscriminator(ssd: ScalaService, union: ScalaUnion, unions: Seq[ScalaUnion]): String = {
    val disc = generateUnionDiscriminatorTrait(union) match {
      case None => ""
      case Some(code) => s"\n\n$code"
    }

    ScalaGeneratorUtil.scaladoc(union.description, Nil) + generateUnionTrait(ssd, union, unions) + disc
  }

  def generateUnionTrait(ssd: ScalaService, union: ScalaUnion, unions: Seq[ScalaUnion]): String = {
    // TODO: handle primitive types
    val body = Seq(
      ScalaUtil.deprecationString(union.deprecation).trim match {
        case "" => None
        case v => Some(v)
      },

      Some(s"sealed trait ${union.name}" + ScalaUtil.extendsClause(
        className = union.name,
        interfaces = union.interfaces,
        unions = unions.map(_.name),
      ).getOrElse(" extends _root_.scala.Product with _root_.scala.Serializable"))
    ).flatten.mkString("\n")
    val fieldBody = (union.discriminatorField.toList ++ fields(ssd, union)).map { f =>
      s"def ${f.name}: ${f.datatype.name}"
    }.mkString("\n")
    withInterfaceFields(body, fieldBody)
  }

  private[this] def fields(ssd: ScalaService, union: ScalaUnion): List[ScalaField] = {
    ssd.findAllInterfaceFields(union.union.interfaces)
  }

  def generateUnionDiscriminatorTrait(union: ScalaUnion): Option[String] = {
    union.discriminator.map { _ =>
      ScalaUnionDiscriminatorGenerator(union).build()
    }
  }

  def generateTraitWithDoc(interface: ScalaInterface): String = {
    ScalaGeneratorUtil.scaladoc(interface.description, interface.fields.map(f => (f.name, f.description))) +
      generateTrait(interface)
  }

  def generateCaseClassWithDoc(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    ScalaGeneratorUtil.scaladoc(model.description, model.fields.map(f => (f.name, f.description))) +
      generateCaseClass(model, unions)
  }

  def generateCaseClass(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    Seq(
      Some(ScalaUtil.deprecationString(model.deprecation).trim).filter(_.nonEmpty),
      Some(s"final case class ${model.name}(${model.argList(unions).getOrElse("")})" + ScalaUtil.extendsClause(
        className = model.name,
        interfaces = model.interfaces,
        unions = unions.map(_.name),
      ).getOrElse("") + discriminatorFieldValue(model, unions).getOrElse(""))
    ).flatten.mkString("\n")
  }

  private[this] def discriminatorFieldValue(model: ScalaModel, unions: Seq[ScalaUnion]): Option[String] = {
    unions.filter(_.discriminatorField.nonEmpty).flatMap { u =>
      discriminatorValue(u, model).map(_.generatorCode)
    }.toList match {
      case Nil => None
      case lines => Some(
        Seq(
          " {",
          "  " + lines.mkString(", "),
         "}"
        ).mkString("\n")
      )
    }
  }

  private[this] def discriminatorValue(union: ScalaUnion, model: ScalaModel): Option[DiscriminatorValue] = {
    union.discriminatorField.flatMap { d =>
      union.types.find(_.name == model.name) match {
        case Some(t) => Some(DiscriminatorValue.TypeModel(d, t))
        case None if model.name == union.undefinedType.model.name => Some(
          DiscriminatorValue.Undefined(d, union.undefinedType)
        )
        case None => None
      }
    }
  }

  def generateTrait(interface: ScalaInterface): String = {
    val fullBody = interface.body match {
      case None => ""
      case Some(b) => s" {\n${b.indentString()}\n}"
    }
    Seq(
      Some(ScalaUtil.deprecationString(interface.deprecation).trim).filter(_.nonEmpty),
      Some(s"trait ${interface.name}$fullBody"),
    ).flatten.mkString("\n")
  }

  def generateEnum(ssd: ScalaService, enum: ScalaEnum): String = {
    ScalaEnums(ssd, enum).build()
  }

  sealed trait DiscriminatorValue {
    def generatorCode: String
  }
  object DiscriminatorValue {
    private[this] def declaration(discriminatorField: ScalaField): String = {
      s"override val ${discriminatorField.field.name}: ${discriminatorField.field.`type`}"
    }

    case class TypeModel(discriminatorField: ScalaField, unionType: ScalaUnionType) extends DiscriminatorValue {
      override def generatorCode: String = {
        s"${declaration(discriminatorField)} = ${discriminatorField.field.`type`}(${ScalaUtil.wrapInQuotes(unionType.discriminatorName)})"
      }
    }
    case class Undefined(discriminatorField: ScalaField, wrapper: UnionTypeUndefinedModelWrapper) extends DiscriminatorValue {
      override def generatorCode: String = {
        s"${declaration(discriminatorField)} = ${discriminatorField.field.`type`}.UNDEFINED(${wrapper.descriptionField.name})"
      }
    }
  }
}
