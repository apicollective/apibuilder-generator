package scala.generator

import scala.models.{ApiBuilderComments, Attributes}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator
import generator.ServiceFileNames

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

    val undefinedModels = UnionTypeUndefinedModel(ssd).models match {
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

    val generatedClasses = Seq(undefinedModels, wrappers).filter(!_.isEmpty) match {
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
      ).mkString("\n")
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
    val fieldBody = ssd.findAllInterfaceFields(union.union.interfaces).map { f =>
      s"def ${f.name}: ${f.datatype.name}"
    }.mkString("\n")
    withInterfaceFields(body, fieldBody)
  }

  def generateUnionDiscriminatorTrait(union: ScalaUnion): Option[String] = {
    union.discriminator.map { _ =>
      ScalaUnionDiscriminator(union).build()
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
      ).getOrElse(""))
    ).flatten.mkString("\n")
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

}
