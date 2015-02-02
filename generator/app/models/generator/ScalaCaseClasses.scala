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
      ssd.models.map { generateCaseClass(_) }.mkString("\n\n").indent(2),
      "",
      genEnums(ssd.enums).indent(2)
    ).mkString("\n").trim +
    s"\n\n}"
  }

  private def generateUnionTraits(models: Seq[ScalaModel], union: ScalaUnion): String = {
    // TODO: handle primitive types

    // For union types across only models, find all common fields and bubble up to the trait
    case class FieldType(name: String, datatype: ScalaDatatype)

    val unionModels = union.types.flatMap { t => models.find(_.qualifiedName == t.name) }

    val commonFields = if (!unionModels.isEmpty && unionModels.map(_.name).sorted == union.types.map(_.primitive.shortName).sorted) {
      val allFields = unionModels.flatMap { _.fields.map( f => s"${f.definition(f.name)}" ) }.groupBy(_)
      val selected = allFields.filter(_._2.size == unionModels.size).map(_._1).toSeq

      unionModels.head.fields.filter(field => selected.contains(field.name)) match {
        case Nil => ""
        case fields => {
          Seq(
            " {",
            fields.map { f => s"def ${f.definition(f.name)}" }.mkString("\n").indent(2),
            "}"
          ).mkString("\n")
        }
      }
    } else {
      ""
    }

    union.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"sealed trait ${union.name}$commonFields"
  }

  def generateCaseClass(model: ScalaModel): String = {
    val extendsClause = model.unions match {
      case Nil => ""
      case unions => " extends " + unions.map(_.name).mkString(" with ")
    }

    model.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
    s"case class ${model.name}(${model.argList.getOrElse("")})" + extendsClause
  }

  private def generatePlayEnums(enums: Seq[ScalaEnum]): String = {
    enums.map { ScalaEnums(_).build }.mkString("\n\n")
  }

}
