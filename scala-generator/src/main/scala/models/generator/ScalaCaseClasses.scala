package scala.generator

import scala.models.ApidocComments
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import com.bryzek.apidoc.spec.v0.models.Service
import lib.Text._
import lib.generator.CodeGenerator
import generator.ServiceFileNames

object ScalaCaseClasses extends ScalaCaseClasses

trait ScalaCaseClasses extends CodeGenerator {

  private[this] val MaxNumberOfFields = 21

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = invoke(form, addHeader = true)

  def modelsWithTooManyFieldsErrors(service: Service): Seq[String] = {
    service.models.filter(_.fields.size > MaxNumberOfFields) match {
      case Nil => Nil
      case invalidModels => {
        Seq(s"One or more models has more than $MaxNumberOfFields fields. This generator relies on scala case classes and play json serialization which do not yet support a larger number of fields. Models with too many fields: " + invalidModels.map(_.name).mkString(", "))
      }
    }
  }

  def invoke(
    form: InvocationForm,
    addHeader: Boolean = true
  ): Either[Seq[String], Seq[File]] = {
    modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(generateCode(form, addHeader))
      case errors => Left(errors)
    }
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
      ssd.unions.map { generateUnionTraitWithDoc }.mkString("\n\n").indent(2),
      "",
      ssd.models.map { m => generateCaseClassWithDoc(m, ssd.unionsForModel(m)) }.mkString("\n\n").indent(2),
      generatedClasses,
      ssd.enums.map { generateEnum(ssd, _) }.mkString("\n\n").indent(2)
    ).mkString("\n").trim +
    s"\n\n}"

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Models", source, Some("Scala")))
  }

  def generateUnionTraitWithDoc(union: ScalaUnion): String = {
    generateScalaDoc(union.description) + generateUnionTrait(union)
  }

  def generateUnionTrait(union: ScalaUnion): String = {
    // TODO: handle primitive types
    s"${ScalaUtil.deprecationString(union.deprecation)}sealed trait ${union.name}"
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
