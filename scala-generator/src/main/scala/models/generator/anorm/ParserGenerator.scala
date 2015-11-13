package scala.generator.anorm

import scala.generator.{ScalaModel, ScalaService}
import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
import lib.Text._

object ParserGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    generateCode(form.service) match {
      case None => {
        Left(Seq("No models were found and thus no parsers were generated"))
      }
      case Some(code) => {
        Right(
          Seq(
            File(
              name = "Parsers.scala",
              contents = code
            )
          )
        )
      }
    }
  }

  private[this] def generateCode(service: Service): Option[String] = {
    val ssd = new ScalaService(service)

    ssd.models.map(generateModel(_)) match {
      case Nil => {
        None
      }
      case models => {
        Some(
          Seq(
            "import anorm._",
            s"package ${ssd.namespaces.anorm} {",
            models.mkString("\n").indent(2),
            "}"
          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] def generateModel(model: ScalaModel): String = {
    Seq(
      s"object ${model.name} {",
      generateModelParserByPrefix(model).indent(2),
      generateModelParser(model).indent(2),
      "}"
    ).mkString("\n\n")
  }

  private[this] def generateModelParserByPrefix(model: ScalaModel): String = {
    Seq(
      """def parserByPrefix(prefix: String, separator: String = ".") = parser(""",
      model.fields.map { f => f.name + """ = s"${prefix}${separator}""" + f.name + "\"" }.mkString(",\n").indent(2),
      ")"
    ).mkString("\n")
  }

  private[this] def generateModelParser(model: ScalaModel): String = {
    Seq(
      "def parser(",
      model.fields.map { f => s"${f.name}: String" }.mkString(",\n").indent(2),
      s"): RowParser[${model.qualifiedName}] = {",
      "//TODO".indent(2),
      "}"
    ).mkString("\n")
  }

}
