package scala.generator.anorm

import scala.generator.{Namespaces, ScalaDatatype, ScalaEnum, ScalaField, ScalaModel, ScalaPrimitive, ScalaService}
import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
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
            ServiceFileNames.toFile(
              form.service.namespace,
              form.service.organization.key,
              form.service.application.key,
              form.service.version,
              "Parsers",
              code,
              Some("Scala")
            )
          )
        )
      }
    }
  }

  private[this] def generateCode(service: Service): Option[String] = {
    val ssd = new ScalaService(service)

    Seq(ssd.models, ssd.enums).flatten.isEmpty match {
      case true => {
        None
      }
      case false => {
        Some(
          Seq(
            "import anorm._",
            s"package ${ssd.namespaces.anormParsers} {",
            Seq(
              ssd.enums.map(generateEnum(_)) match {
                case Nil => None
                case els => Some(els.mkString("\n"))
              },
              ssd.models.map(generateModel(_)) match {
                case Nil => None
                case els => Some(els.mkString("\n"))
              }
            ).flatten.mkString("\n\n").indent(2),
            "}"
          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] def generateEnum(enum: ScalaEnum): String = {
    Seq(
      s"object ${enum.name} {",
      Seq(
        generateEnumMappings(enum),
        generateEnumParser(enum)
      ).mkString("\n\n").indent(2),
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateModel(model: ScalaModel): String = {
    Seq(
      s"object ${model.name} {",
      Seq(
        generateModelMappings(model),
        generateModelParser(model)
      ).mkString("\n\n").indent(2),
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateModelMappings(model: ScalaModel): String = {
    Seq(
      Seq(
        "case class Mappings(",
        model.fields.map { f => s"${f.name}: ${modelFieldParameterType(f.name, f.datatype)}" }.mkString("\n").indent(2),
        ")"
      ).mkString("\n"),
      "object Mappings {",
      Seq(
        "def table(table: String) = Mappings(",
        model.fields.map { f => f.name + " = " + modelFieldParameterTableDefault(f.datatype, f.originalName) }.mkString(",\n").indent(2),
        ")"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  private[this] def generateModelParser(model: ScalaModel): String = {
    Seq(
      s"def table(table: String) = parser(Mappings.table(table))",
      "",
      s"def parser(mappings: Mappings): RowParser[${model.qualifiedName}] = {",
      Seq(
        model.fields.map { generateRowParser(model, _) }.mkString(" ~\n") + " map {",
        Seq(
          "case " + model.fields.map(_.name).mkString(" ~ ") + " => {",
          Seq(
            s"${model.qualifiedName}(",
            model.fields.map { f =>
              s"${f.name} = ${f.name}"
            }.mkString(",\n").indent(2),
            ")"
          ).mkString("\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n")
  }

  @scala.annotation.tailrec
  private[this] def modelFieldParameterType(fieldName: String, datatype: ScalaDatatype): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) => {
        s"""String = "$fieldName""""
      }
      case ScalaDatatype.List(inner) => {
        modelFieldParameterType(fieldName, inner)
      }
      case ScalaDatatype.Map(inner) => {
        modelFieldParameterType(fieldName, inner)
      }
      case ScalaDatatype.Option(inner) => {
        modelFieldParameterType(fieldName, inner)
      }
      case ScalaPrimitive.Model(namespaces, name) => {
        s"""RowParser[${namespaces.models}.$name] = ${namespaces.anormParsers}.$name.prefix("${fieldName}_")"""
      }
      case ScalaPrimitive.Union(namespaces, name) => {
        s"""RowParser[${namespaces.models}.$name] = ${namespaces.anormParsers}.$name.prefix("${fieldName}_")"""
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def modelFieldParameterTableDefault(datatype: ScalaDatatype, name: String): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) => {
        "s\"$table." + name + "\""
      }
      case ScalaDatatype.List(inner) => {
        modelFieldParameterTableDefault(inner, name)
      }
      case ScalaDatatype.Map(inner) => {
        modelFieldParameterTableDefault(inner, name)
      }
      case ScalaDatatype.Option(inner) => {
        modelFieldParameterTableDefault(inner, name)
      }
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) => {
        "TODO(s\"${table}_" + name + "\")"
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def modelFieldParameterNewParserDefault(datatype: ScalaDatatype, name: String): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) => {
        "s\"${prefix}_" + name + "\""
      }
      case ScalaDatatype.List(inner) => {
        modelFieldParameterNewParserDefault(inner, name)
      }
      case ScalaDatatype.Map(inner) => {
        modelFieldParameterNewParserDefault(inner, name)
      }
      case ScalaDatatype.Option(inner) => {
        modelFieldParameterNewParserDefault(inner, name)
      }
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) => {
        "TODO(s\"${prefix}_" + name + "\")"
      }
    }
  }

  private[this] def generateRowParser(model: ScalaModel, field: ScalaField): String = {
    generateRowParser(model, field, field.datatype)
  }

  private[this] def generateRowParser(model: ScalaModel, field: ScalaField, datatype: ScalaDatatype): String = {
    datatype match {
      case f @ ScalaPrimitive.Boolean => s"SqlParser.bool(mappings.${field.name})"
      case f @ ScalaPrimitive.Double => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Integer => s"SqlParser.int(mappings.${field.name})"
      case f @ ScalaPrimitive.Long => s"SqlParser.long(mappings.${field.name})"
      case f @ ScalaPrimitive.DateIso8601 => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.DateTimeIso8601 => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Decimal => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Object => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.String => s"SqlParser.str(mappings.${field.name})"
      case f @ ScalaPrimitive.Unit => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Uuid => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaDatatype.List(inner) => {
        // TODO recurse on inner.datatype
        s"SqlParser.list[${inner.name}](mappings.${field.name})"
      }
      case f @ ScalaDatatype.Map(inner) => {
        // TODO: pull out inner.datatype and turn el.last into that type
        s"SqlParser.list[${inner.name}](mappings.${field.name}).sliding(2, 2).map { el => (el.head.toString -> el.last) }.toMap"
      }
      case f @ ScalaDatatype.Option(inner) => {
        generateRowParser(model, field, inner) + ".?"
      }
      case ScalaPrimitive.Model(ns, name) => {
        s"""${ns.anormParsers}.$name.Parsers.parser(mappings.${field.name})"""
      }
      case ScalaPrimitive.Enum(ns, name) => {
        s"""${ns.anormParsers}.$name.Parsers.parser(mappings.${field.name})"""
      }
      case ScalaPrimitive.Union(ns, name) => {
        s"""${ns.anormParsers}.$name.Parsers.parser(mappings.${field.name})"""
      }
    }
  }

  private[this] def generatePrimitiveRowParser(fieldName: String, datatype: ScalaPrimitive) = {
    s"SqlParser.get[${datatype.fullName}](mappings.$fieldName)"
  }

  private[this] def generateEnumMappings(enum: ScalaEnum): String = {
    "TODO"
  }

  private[this] def generateEnumParser(enum: ScalaEnum): String = {
    Seq(
      s"def newParser(name: String) = parser(name)",
      "",
      "def parserByTable(table: String) = parser(s\"$table." + enum.enum.name + "\")",
      "",
      s"def parser(name: String): RowParser[${enum.qualifiedName}] = {",
      s"  SqlParser.str(name) map {",
      s"    case value => ${enum.qualifiedName}(value)",
      s"  }",
      s"}"
    ).mkString("\n")
  }

}
