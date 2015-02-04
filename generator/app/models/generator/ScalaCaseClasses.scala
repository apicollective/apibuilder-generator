package generator

import models.ApidocHeaders
import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.Service
import lib.Text._

object ScalaCaseClasses extends CodeGenerator {

  override def invoke(form: InvocationForm): String = invoke(form, generatePlayEnums, addHeader = true)

  def invoke(
    form: InvocationForm,
    genEnums: Seq[ScalaEnum] => String = generatePlayEnums,
    addHeader: Boolean = true
  ): String = {
    val ssd = new ScalaService(form.service)

    val header = addHeader match {
      case false => ""
      case true => ApidocHeaders(form.userAgent).toJavaString() + "\n"
    }

    s"${header}package ${ssd.namespaces.models} {\n\n  " +
    Seq(
      ssd.unions.map { generateUnionTraits(ssd.models, _) }.mkString("\n\n").indent(2),
      "",
      ssd.models.map { m => generateCaseClass(m, ssd.unionsForModel(m)) }.mkString("\n\n").indent(2),
      "",
      genEnums(ssd.enums).indent(2)
    ).mkString("\n").trim +
    s"\n\n}"
  }

  private def generateUnionTraits(models: Seq[ScalaModel], union: ScalaUnion): String = {
    // TODO: handle primitive types

    union.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"sealed trait ${union.name}"
  }

  def generateCaseClass(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    val extendsClause = unions match {
      case Nil => ""
      case unions => " extends " + unions.map(_.name).sorted.mkString(" with ")
    }

    model.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"case class ${model.name}(${model.argList.getOrElse("")})" + extendsClause
  }

  private def generatePlayEnums(enums: Seq[ScalaEnum]): String = {
    enums.map { ScalaEnums(_).build }.mkString("\n\n")
  }

}
