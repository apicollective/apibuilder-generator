package scala.generator

import scala.models.ApidocComments
import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models.Service
import lib.Text._
import lib.generator.CodeGenerator

object ScalaCaseClasses extends CodeGenerator {

  private[this] val MaxNumberOfFields = 21

  override def invoke(form: InvocationForm): Either[Seq[String], String] = invoke(form, addHeader = true)

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
  ): Either[Seq[String], String] = {
    modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(generateCode(form, addHeader))
      case errors => Left(errors)
    }
  }

  def generateCode(
    form: InvocationForm,
    addHeader: Boolean = true
  ): String = {
    val ssd = new ScalaService(form.service)

    val header = addHeader match {
      case false => ""
      case true => ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
    }

    val undefinedModels = UnionTypeUndefinedModel(ssd).models match {
      case Nil => ""
      case wrappers => {
        wrappers.map { w => generateCaseClass(w.model, Seq(w.union)) }.mkString("\n\n").indent(2) + "\n"
      }
    }

    val wrappers = PrimitiveWrapper(ssd).wrappers match {
      case Nil => ""
      case wrappers => {
        wrappers.map { w => generateCaseClass(w.model, Seq(w.union)) }.mkString("\n\n").indent(2) + "\n"
      }
    }

    val generatedClasses = Seq(undefinedModels, wrappers).filter(!_.isEmpty) match {
      case Nil => ""
      case code => "\n" + code.mkString("\n\n")
    }

    s"${header}package ${ssd.namespaces.models} {\n\n  " +
    Seq(
      ssd.unions.map { generateUnionTraits(ssd.models, _) }.mkString("\n\n").indent(2),
      "",
      ssd.models.map { m => generateCaseClass(m, ssd.unionsForModel(m)) }.mkString("\n\n").indent(2),
      generatedClasses,
      generatePlayEnums(ssd).indent(2)
    ).mkString("\n").trim +
    s"\n\n}"
  }

  private def generateUnionTraits(models: Seq[ScalaModel], union: ScalaUnion): String = {
    // TODO: handle primitive types

    union.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"sealed trait ${union.name}"
  }

  def generateCaseClass(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    model.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"case class ${model.name}(${model.argList.getOrElse("")})" + ScalaUtil.extendsClause(unions.map(_.name)).map(s => s" $s").getOrElse("")
  }

  private def generatePlayEnums(ssd: ScalaService): String = {
    ssd.enums.map { ScalaEnums(ssd, _).build }.mkString("\n\n")
  }

}
