package scala.generator.anorm

import scala.generator.{Namespaces, ScalaDatatype, ScalaEnum, ScalaField, ScalaModel, ScalaPrimitive, ScalaService, ScalaUtil}
import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text
import lib.Text._

object ParserGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = new ScalaService(form.service)

    generateCode(ssd) match {
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
              "Conversions",
              Conversions.code(ssd.namespaces, ssd.unions),
              Some("Scala")
            ),
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

  private[this] def generateCode(ssd: ScalaService): Option[String] = {
    Seq(ssd.models, ssd.enums).flatten.isEmpty match {
      case true => {
        None
      }
      case false => {
        Some(
          Seq(
            "import anorm._",
            s"package ${ssd.namespaces.anormParsers} {",
            s"  import ${ssd.namespaces.anormConversions}.Json._",
            Seq(
              ssd.enums.map(generateEnum(_)),
              ssd.models.map(generateModel(_))
            ).filter(!_.isEmpty).flatten.mkString("\n").indent(2),
            "}"
          ).mkString("\n\n")
        )
      }
    }
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
        model.fields.map { f => s"${f.name}: ${modelFieldParameterType(f.datatype, f.originalName)}" }.mkString(",\n").indent(2),
        ")"
      ).mkString("\n"),
      "object Mappings {",
      Seq(
        """val base = prefix("", "")""",
        """def table(table: String) = prefix(table, ".")""",
        Seq(
          "def prefix(prefix: String, sep: String) = Mappings(",
          model.fields.map { f => f.name + " = " + modelFieldParameterDefault(f.datatype, f.originalName) }.mkString(",\n").indent(2),
          ")"
        ).mkString("\n")
      ).mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  private[this] def generateModelParser(model: ScalaModel): String = {
    Seq(
      s"""def table(table: String) = parser(Mappings.prefix(table, "."))""",
      "",
      s"def parser(mappings: Mappings): RowParser[${model.qualifiedName}] = {",
      Seq(
        model.fields.map { generateRowParser(_) }.mkString(" ~\n") + " map {",
        Seq(
          "case " + model.fields.map(parserName(_)).mkString(" ~ ") + " => {",
          Seq(
            s"${model.qualifiedName}(",
            model.fields.map { f =>
              s"${f.name} = ${parserName(f)}"
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
  private[this] def modelFieldParameterType(datatype: ScalaDatatype, fieldName: String): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        s"""String = "$fieldName""""
      }
      case ScalaDatatype.List(inner) => {
        modelFieldParameterType(inner, fieldName)
      }
      case ScalaDatatype.Map(inner) => {
        modelFieldParameterType(inner, fieldName)
      }
      case ScalaDatatype.Option(inner) => {
        modelFieldParameterType(inner, fieldName)
      }
      case ScalaPrimitive.Model(namespaces, name) => {
        s"${namespaces.anormParsers}.$name.Mappings"
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def modelFieldParameterDefault(datatype: ScalaDatatype, name: String): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
        "s\"${prefix}${sep}" + name + "\""
      }
      case ScalaDatatype.List(inner) => {
        modelFieldParameterDefault(inner, name)
      }
      case ScalaDatatype.Map(inner) => {
        modelFieldParameterDefault(inner, name)
      }
      case ScalaDatatype.Option(inner) => {
        modelFieldParameterDefault(inner, name)
      }
      case ScalaPrimitive.Model(ns, className) => {
        s"""${ns.anormParsers}.$className.Mappings.prefix(Seq(prefix, "$name").filter(!_.isEmpty).mkString("_"), "_")"""
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def modelFieldParameterNewParserDefault(datatype: ScalaDatatype, name: String): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
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
      case ScalaPrimitive.Model(_, _) => {
        "TODO(s\"${prefix}_" + name + "\")"
      }
    }
  }

  private[this] def generateRowParser(field: ScalaField): String = {
    generateRowParser(field.name, field.datatype)
  }

  private[this] def generateRowParser(fieldName: String, datatype: ScalaDatatype): String = {
    datatype match {
      case f @ ScalaPrimitive.Boolean => s"SqlParser.bool(mappings.${fieldName})"
      case f @ ScalaPrimitive.Double => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Integer => s"SqlParser.int(mappings.${fieldName})"
      case f @ ScalaPrimitive.Long => s"SqlParser.long(mappings.${fieldName})"
      case f @ ScalaPrimitive.DateIso8601 => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.DateTimeIso8601 => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Decimal => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Object => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.String => s"SqlParser.str(mappings.${fieldName})"
      case f @ ScalaPrimitive.Unit => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Uuid => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaDatatype.List(inner) => {
        s"SqlParser.get[Seq[${inner.name}]](mappings.${fieldName})"
      }
      case f @ ScalaDatatype.Map(inner) => {
        s"SqlParser.get[Map[String, ${inner.name}]](mappings.${fieldName})"
      }
      case f @ ScalaDatatype.Option(inner) => {
        generateRowParser(fieldName, inner) + ".?"
      }
      case ScalaPrimitive.Enum(ns, name) => {
        s"""${ns.anormParsers}.$name.parser(${ns.anormParsers}.$name.Mappings(mappings.${fieldName}))"""
      }
      case ScalaPrimitive.Model(ns, name) => {
        s"""${ns.anormParsers}.$name.parser(mappings.${fieldName})"""
      }
      case ScalaPrimitive.Union(ns, name) => {
        s"SqlParser.get[${ns.unions}.$name](mappings.${fieldName})"
      }
    }
  }

  private[this] def generatePrimitiveRowParser(fieldName: String, datatype: ScalaPrimitive) = {
    s"SqlParser.get[${datatype.fullName}](mappings.$fieldName)"
  }

  private[this] def generateEnum(enum: ScalaEnum): String = {
    Seq(
      s"object ${enum.name} {",
      Seq(
        "case class Mappings(value: String)",
        "object Mappings {",
        Seq(
          """val base = prefix("", "")""".indent(2),
          """def table(table: String) = prefix(table, ".")""".indent(2),
          Seq(
            "def prefix(prefix: String, sep: String) = Mappings(",
            """  value = s"${prefix}${sep}value"""",
            ")"
          ).mkString("\n").indent(2)
        ).mkString("\n\n"),
        "}",
        """def table(table: String) = parser(Mappings.prefix(table, "."))""",
        Seq(
          s"def parser(mappings: Mappings): RowParser[${enum.qualifiedName}] = {",
          s"  SqlParser.str(mappings.value) map {",
          s"    case value => ${enum.qualifiedName}(value)",
          s"  }",
          s"}"
        ).mkString("\n")
      ).mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  /**
    * Parsers require special quoting of keywords - if the field name
    * is a keyword, we append Instance (back ticks will not compile)
    */
  private[this] def parserName(field: ScalaField): String = {
    ScalaUtil.isKeyword(field.originalName) match {
      case true => Text.snakeToCamelCase(field.originalName) + "Instance"
      case false => field.name
    }
  }

}
