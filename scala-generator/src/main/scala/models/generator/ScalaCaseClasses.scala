package scala.generator

import scala.models.ApidocComments
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import com.bryzek.apidoc.spec.v0.models.Service
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
    Right(generateCode(form, addHeader))
  }

  def generateCode(
    form: InvocationForm,
    addHeader: Boolean = true,
    additionalImports: Seq[String] = Seq.empty
  ): Seq[File] = {
    val ssd = new ScalaService(form.service)

    val header = addHeader match {
      case false => ""
      case true => ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
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

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Models", source, Some("Scala")))
  }

  def generateUnionTraitWithDocAndDiscriminator(union: ScalaUnion): String = {
    val disc = generateUnionDiscriminatorTrait(union) match {
      case None => ""
      case Some(code) => s"\n\n$code"
    }

    generateScalaDoc(union.description) + generateUnionTrait(union) + disc
  }

  def generateUnionTrait(union: ScalaUnion): String = {
    // TODO: handle primitive types
    s"${ScalaUtil.deprecationString(union.deprecation)}sealed trait ${union.name} extends _root_.scala.Product with _root_.scala.Serializable"
  }

  def generateUnionDiscriminatorTrait(union: ScalaUnion): Option[String] = {
    union.discriminator.map { disc =>
      ScalaUnionDiscriminator(union).build()
    }
  }

  def generateCaseClassWithDoc(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    generateScalaDoc(model.description) + generateCaseClass(model, unions)
  }

  def generateCaseClass(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    s"${ScalaUtil.deprecationString(model.deprecation)}case class ${model.name}(${model.argList.getOrElse("")})" + ScalaUtil.extendsClause(unions.map(_.name)).map(s => s" $s").getOrElse("")
  }

  def generateEnum(ssd: ScalaService, enum: ScalaEnum): String = {
    ScalaEnums(ssd, enum).build
  }

  def generateScalaDoc(description: Option[String]) = description.fold("")(d => ScalaUtil.textToComment(d) + "\n")
}
