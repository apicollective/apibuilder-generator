package generator

import models.ApidocHeaders
import com.gilt.apidocgenerator.models.{InvocationForm, Service}
import lib.Text._

object ScalaCaseClasses extends CodeGenerator {

  override def invoke(
    form: InvocationForm,
    genEnums: Map[String, ScalaEnum] => String = generatePlayEnums,
    addHeader: Boolean = true
  ): String = {
    val ssd = new ScalaService(form.service)

    val header = addHeader match {
      case false => ""
      case true => ApidocHeaders(form.userAgent).toJavaString() + "\n"
    }

    s"${header}package ${ssd.modelPackageName} {\n\n  " +
    Seq(
      ssd.models.map { case (name, model) =>
        generateCaseClass(name, model)
      }.mkString("\n\n").indent(2),
      "",
      genEnums(ssd.enums).indent(2)
    ).mkString("\n").trim +
    s"\n\n}"
  }

  def generateCaseClass(name: String, model: ScalaModel): String = {
    model.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"case class $name(${model.argList.getOrElse("")})"
  }

  private def generatePlayEnums(enums: Map[String, ScalaEnum]): String = {
    enums.map { case(name, enum) =>
      ScalaEnums(name, enum).build
    }.mkString("\n\n")
  }

}
