package scala.generator

import scala.models.ApidocComments
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Service
import lib.Text._
import lib.generator.{CodeGenerator, GeneratorUtil}
import generator.ServiceFileNames

object ScalaCaseClasses extends ScalaCaseClasses

trait ScalaCaseClasses extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = invoke(form, addHeader = true)

  def invoke(
    form: InvocationForm,
    addHeader: Boolean = true
  ): Either[Seq[String], Seq[File]] = {
    val ssd = new ScalaService(form.service)
    Right(generateCode(ssd, form.userAgent, addHeader))
  }

  def generateCode(
    ssd: ScalaService,
    userAgent: Option[String],
    addHeader: Boolean = true,
    additionalImports: Seq[String] = Seq.empty
  ): Seq[File] = {
    val header = if (addHeader) {
      ApidocComments(ssd.service.version, userAgent).toJavaString() + "\n"
    } else {
      ""
    }

    val undefinedModels = UnionTypeUndefinedModel(ssd).models match {
      case Nil => ""
      case wrappers => {
        wrappers.map { w => generateCaseClassWithDoc(w.model, Seq(w.union)) }.mkString("\n\n").indent(2) + "\n"
      }
    }

    val wrappers = PrimitiveWrapper(ssd).wrappers match {
      case Nil => ""
      case wrappers => {
        wrappers.map { w => generateCaseClassWithDoc(w.model, Seq(w.union)) }.mkString("\n\n").indent(2) + "\n"
      }
    }

    val generatedClasses = Seq(undefinedModels, wrappers).filter(!_.isEmpty) match {
      case Nil => ""
      case code => "\n" + code.mkString("\n\n")
    }

    val source = s"${header}package ${ssd.namespaces.models} {\n\n  " +
    Seq(
      additionalImports.mkString("\n").indent(2),
      ssd.unions.map { generateUnionTraitWithDocAndDiscriminator }.mkString("\n\n").indent(2),
      "",
      ssd.models.map { m => generateCaseClassWithDoc(m, ssd.unionsForModel(m)) }.mkString("\n\n").indent(2),
      generatedClasses,
      ssd.enums.map { generateEnum(ssd, _) }.mkString("\n\n").indent(2)
    ).mkString("\n").trim +
    s"\n\n}"

    Seq(ServiceFileNames.toFile(ssd.service.namespace, ssd.service.organization.key, ssd.service.application.key, ssd.service.version, "Models", source, Some("Scala")))
  }

  def generateUnionTraitWithDocAndDiscriminator(union: ScalaUnion): String = {
    val disc = generateUnionDiscriminatorTrait(union) match {
      case None => ""
      case Some(code) => s"\n\n$code"
    }

    ScalaGeneratorUtil.scaladoc(union.description, Nil) + generateUnionTrait(union) + disc
  }

  def generateUnionTrait(union: ScalaUnion): String = {
    // TODO: handle primitive types

    val declaration = s"sealed trait ${union.name} extends _root_.scala.Product with _root_.scala.Serializable"
    val code = union.discriminator match {
      case None => declaration
      case Some(disc) => {
        Seq(
          s"$declaration {",
          s"  def ${discriminatorMethodName(disc)}: ${union.ssd.namespaces.models}.${ScalaUnionDiscriminator(union).className}",
          "}"
        ).mkString("\n\n")
      }
    }

    Seq(
      ScalaUtil.deprecationString(union.deprecation).trim match {
        case "" => None
        case v => Some(v)
      },
      Some(code)
    ).flatten.mkString("\n")
  }

  def generateUnionDiscriminatorTrait(union: ScalaUnion): Option[String] = {
    union.discriminator.map { _ =>
      ScalaUnionDiscriminator(union).build()
    }
  }

  def generateCaseClassWithDoc(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    ScalaGeneratorUtil.scaladoc(model.description, model.fields.map(f => (f.name, f.description))) +
      generateCaseClass(model, unions)
  }

  def generateCaseClass(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    val body = defineDiscriminators(model, unions).map { code => s" {\n\n$code\n\n}" }
    Seq(
      Some(ScalaUtil.deprecationString(model.deprecation).trim).filter(_.nonEmpty),
      Some(s"case class ${model.name}(${model.argList.getOrElse("")})" + ScalaUtil.extendsClause(unions.map(_.name)).map(s => s" $s").getOrElse("") + body.getOrElse(""))
    ).flatten.mkString("\n")
  }

  private[this] def defineDiscriminators(model: ScalaModel, unions: Seq[ScalaUnion]): Option[String] = {
    unions.flatMap { union =>
      union.discriminator.map { disc =>
        val base = s"${union.ssd.namespaces.models}.${ScalaUnionDiscriminator(union).className}"
        s"  override val ${discriminatorMethodName(disc)}: $base = $base.${model.name}"
      }
    }.toList match {
      case Nil => None
      case functions => {
        Some(functions.mkString("\n\n"))
      }
    }
  }

  def generateEnum(ssd: ScalaService, enum: ScalaEnum): String = {
    ScalaEnums(ssd, enum).build()
  }

  private[this] def discriminatorMethodName(disc: String): String = {
    ScalaUtil.quoteNameIfKeyword(ScalaUtil.toVariable(disc))
  }
}
