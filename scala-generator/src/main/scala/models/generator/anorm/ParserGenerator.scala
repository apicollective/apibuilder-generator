package scala.generator.anorm

import scala.generator.{ScalaField, ScalaModel, ScalaPrimitive, ScalaService}
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
      Seq(
        model.fields.map { generateRowParser(_) }.mkString(" ~\n") + " map {",
        Seq(
          "case " + model.fields.map(_.name).mkString(" ") + " => {",
          Seq(
            s"${model.qualifiedName}(",
            model.fields.map { f =>
              s"${f.name} = ${f.name}"
            }.mkString("\n").indent(2),
            ")"
          ).mkString("\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n")
  }

  private[this] def generateRowParser(field: ScalaField) = {
    field.datatype match {
      case f @ ScalaPrimitive.Boolean => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Double => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Integer => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Long => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.DateIso8601 => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.DateTimeIso8601 => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Decimal => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Object => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.String => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Unit => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Uuid => generatePrimitiveRowParser(field.name, f)
      // TODO: ScalaPrimitive. List, Map Option
      case ScalaPrimitive.Model(ns, name) => {
        """$ns.$name.parserByPrefix(todo, "_")"""
      }
      case ScalaPrimitive.Enum(ns, name) => {
        """$ns.$name.parserByPrefix(todo, "_")"""
      }
      case ScalaPrimitive.Union(ns, name) => {
        """$ns.$name.parserByPrefix(todo, "_")"""
      }
    }
  }

  private[this] def generatePrimitiveRowParser(field: String, datatype: ScalaPrimitive) = {
    s"SqlParser.get[${datatype.fullName}]($field)"
  }

}
