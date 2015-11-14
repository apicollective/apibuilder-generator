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
            s"package ${ssd.namespaces.anorm} {",
            Seq(
              Some(s"package ${Namespaces.Parsers} {"),
              ssd.enums.map(generateEnum(_)) match {
                case Nil => None
                case els => Some(els.mkString("\n").indent(2))
              },
              ssd.models.map(generateModel(_)) match {
                case Nil => None
                case els => Some(els.mkString("\n").indent(2))
              },
              Some("}")
            ).flatten.mkString("\n\n").indent(2),
            AnormUtilPackage.indent(2),
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
        generateEnumParser(enum)
      ).mkString("\n\n").indent(2),
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateModel(model: ScalaModel): String = {
    Seq(
      s"object ${model.name} {",
      Seq(
        generateModelNewParser(model),
        generateModelParserByTable(model),
        generateModelParser(model)
      ).mkString("\n\n").indent(2),
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateModelNewParser(model: ScalaModel): String = {
    Seq(
      "def newParser(config: util.Config) = {",
      "  config match {",
      "    case util.Config.Prefix(prefix) => parser(",
      model.fields.map { f => f.name + " = " + modelFieldParameterNewParserDefault(f.datatype, f.name) }.mkString(",\n").indent(6),
      "    )",
      "  }",
      "}"
    ).mkString("\n")
  }

  private[this] def generateModelParserByTable(model: ScalaModel): String = {
    Seq(
      """def parserByTable(table: String) = parser(""",
      model.fields.map { f => f.name + " = " + modelFieldParameterTableDefault(f.datatype, f.name) }.mkString(",\n").indent(2),
      ")"
    ).mkString("\n")
  }

  private[this] def generateModelParser(model: ScalaModel): String = {
    Seq(
      "def parser(",
      model.fields.map { f => s"${f.name}: ${modelFieldParameterType(f.datatype)}" }.mkString(",\n").indent(2),
      s"): RowParser[${model.qualifiedName}] = {",
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
  private[this] def modelFieldParameterType(datatype: ScalaDatatype): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) => {
        "String"
      }
      case ScalaDatatype.List(inner) => {
        modelFieldParameterType(inner)
      }
      case ScalaDatatype.Map(inner) => {
        modelFieldParameterType(inner)
      }
      case ScalaDatatype.Option(inner) => {
        modelFieldParameterType(inner)
      }
      case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) => {
        "util.Config"
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
        "util.Config.Prefix(s\"${table}_" + name + "\")"
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
        "util.Config.Prefix(s\"${prefix}_" + name + "\")"
      }
    }
  }

  private[this] def generateRowParser(model: ScalaModel, field: ScalaField): String = {
    generateRowParser(model, field, field.datatype)
  }

  private[this] def generateRowParser(model: ScalaModel, field: ScalaField, datatype: ScalaDatatype): String = {
    datatype match {
      case f @ ScalaPrimitive.Boolean => s"SqlParser.bool(${field.name})"
      case f @ ScalaPrimitive.Double => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Integer => s"SqlParser.int(${field.name})"
      case f @ ScalaPrimitive.Long => s"SqlParser.long(${field.name})"
      case f @ ScalaPrimitive.DateIso8601 => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.DateTimeIso8601 => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Decimal => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Object => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.String => s"SqlParser.str(${field.name})"
      case f @ ScalaPrimitive.Unit => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaPrimitive.Uuid => generatePrimitiveRowParser(field.name, f)
      case f @ ScalaDatatype.List(inner) => {
        // TODO recurse on inner.datatype
        s"SqlParser.get[${inner.name}].list(${field.name})"
      }
      case f @ ScalaDatatype.Map(inner) => {
        // TODO: pull out inner.datatype and turn el.last into that type
        s"SqlParser.get[${inner.name}].list(${field.name}).sliding(2, 2).map { el => (el.head.toString -> el.last) }.toMap"
      }
      case f @ ScalaDatatype.Option(inner) => {
        generateRowParser(model, field, inner) + ".?"
      }
      case ScalaPrimitive.Model(ns, name) => {
        s"""${ns.anormParsers}.$name.newParser(${field.name})"""
      }
      case ScalaPrimitive.Enum(ns, name) => {
        s"""${ns.anormParsers}.$name.newParser(${field.name})"""
      }
      case ScalaPrimitive.Union(ns, name) => {
        s"""${ns.anormParsers}.$name.newParser(${field.name})"""
      }
    }
  }

  private[this] def generatePrimitiveRowParser(fieldName: String, datatype: ScalaPrimitive) = {
    s"SqlParser.get[${datatype.fullName}]($fieldName)"
  }

  private[this] def generateEnumParser(enum: ScalaEnum): String = {
    Seq(
      s"def newParser(name: String) = parser(name)",
      "",
      s"def parser(name: String): RowParser[${enum.qualifiedName}] = {",
      s"  SqlParser.str(name) map {",
      s"    case value => ${enum.qualifiedName}(value)",
      s"  }",
      s"}"
    ).mkString("\n")
  }

  private val AnormUtilPackage = """
package util {

  sealed trait Config {
    def name(column: String): String
  }

  object Config {
    case class Prefix(prefix: String) extends Config {
      override def name(column: String): String = s"${prefix}_$column"
    }
  }

}
""".trim

}
