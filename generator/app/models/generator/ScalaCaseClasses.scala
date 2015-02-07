package generator

import models.ApidocHeaders
import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.Service
import lib.Text._

object ScalaCaseClasses extends CodeGenerator {

  override def invoke(form: InvocationForm): String = invoke(form, addHeader = true)

  def invoke(
    form: InvocationForm,
    addHeader: Boolean = true
  ): String = {
    val ssd = new ScalaService(form.service)

    val header = addHeader match {
      case false => ""
      case true => ApidocHeaders(form.userAgent).toJavaString() + "\n"
    }

    val wrappers = PrimitiveWrapper(ssd).models match {
      case Nil => ""
      case models => {
        // TODO: Figure out how to get the unions right here
        "\n" + models.map { m => generateCaseClass(m, ssd.unionsForModel(m)) }.mkString("\n\n").indent(2) + "\n"
      }
    }

    s"${header}package ${ssd.namespaces.models} {\n\n  " +
    Seq(
      ssd.unions.map { generateUnionTraits(ssd.models, _) }.mkString("\n\n").indent(2),
      "",
      ssd.models.map { m => generateCaseClass(m, ssd.unionsForModel(m)) }.mkString("\n\n").indent(2),
      wrappers,
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
