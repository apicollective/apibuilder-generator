package scala.generator.anorm

import scala.generator.{Namespaces, ScalaDatatype, ScalaEnum, ScalaField, ScalaModel, ScalaPrimitive, ScalaService, ScalaUnion, ScalaUtil}
import scala.models.ApidocComments
import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text
import lib.Text._

object ParserGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = new ScalaService(form.service)

    val header = ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"

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
              header ++ Conversions.code(ssd),
              Some("Scala")
            ),
            ServiceFileNames.toFile(
              form.service.namespace,
              form.service.organization.key,
              form.service.application.key,
              form.service.version,
              "Parsers",
              header ++ code,
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
              ssd.models.map(generateModel(_)),
              ssd.unions.map(generateUnion(_))
            ).flatten.map(_.trim).filter(!_.isEmpty).mkString("\n\n").indent(2),
            "}"
          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] def generateModel(model: ScalaModel): String = {
    Seq(
      s"object ${model.name} {",
      generateModelParser(model).indent(2),
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateModelParser(model: ScalaModel): String = {
    Seq(
      Seq(
        s"""def parserWithPrefix(prefix: String, sep: String = "_") = parser(""",
        model.fields.map { f =>
          val argName = parserFieldName(f.originalName, f.datatype)
          s"""${ScalaUtil.quoteNameIfKeyword(argName)} = s"""" + "$prefix${sep}" + s"""${f.originalName}""""
        }.mkString(",\n").indent(2),
        ")"
      ).mkString("\n"),
      "",
      s"def parser(",
      model.fields.map { f =>
        parserFieldDeclaration(f.name, f.datatype, f.originalName)
      }.mkString(",\n").indent(2),
      s"): RowParser[${model.qualifiedName}] = {",
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
  private[this] def parserFieldName(name: String, datatype: ScalaDatatype): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) => {
        ScalaUtil.toVariable(name)
      }
      case ScalaPrimitive.Model(_, _) => {
        ScalaUtil.toVariable(s"${name}Prefix")
      }
      case ScalaPrimitive.Union(_, _) => {
        ScalaUtil.toVariable(s"${name}Prefix")
      }
      case ScalaDatatype.List(inner) => {
        ScalaUtil.toVariable(name)
      }
      case ScalaDatatype.Map(inner) => {
        ScalaUtil.toVariable(name)
      }
      case ScalaDatatype.Option(inner) => {
        parserFieldName(name, inner)
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def parserFieldDeclaration(name: String, datatype: ScalaDatatype, originalName: String): String = {
    datatype match {
      case ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Object | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) | ScalaDatatype.List(_) | ScalaDatatype.Map(_) => {
        s"""$name: String = "$originalName""""
      }
      case ScalaPrimitive.Model(namespaces, name) => {
        val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
        s"""$varName: String = "$originalName""""
      }
      case ScalaPrimitive.Union(namespaces, name) => {
        val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
        s"""$varName: String = "$originalName""""
      }
      case ScalaDatatype.Option(inner) => {
        parserFieldDeclaration(name, inner, originalName)
      }
    }
  }

  private[this] def generateRowParser(field: ScalaField): String = {
    generateRowParser(field.name, field.datatype, field.originalName)
  }

  private[this] def generateRowParser(fieldName: String, datatype: ScalaDatatype, originalName: String): String = {
    datatype match {
      case f @ ScalaPrimitive.Boolean => s"SqlParser.bool($fieldName)"
      case f @ ScalaPrimitive.Double => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Integer => s"SqlParser.int($fieldName)"
      case f @ ScalaPrimitive.Long => s"SqlParser.long($fieldName)"
      case f @ ScalaPrimitive.DateIso8601 => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.DateTimeIso8601 => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Decimal => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Object => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.String => s"SqlParser.str($fieldName)"
      case f @ ScalaPrimitive.Unit => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaPrimitive.Uuid => generatePrimitiveRowParser(fieldName, f)
      case f @ ScalaDatatype.List(inner) => {
        s"SqlParser.get[Seq[${inner.name}]]($fieldName)"
      }
      case f @ ScalaDatatype.Map(inner) => {
        s"SqlParser.get[Map[String, ${inner.name}]]($fieldName)"
      }
      case f @ ScalaDatatype.Option(inner) => {
        generateRowParser(fieldName, inner, originalName) + ".?"
      }
      case ScalaPrimitive.Enum(ns, name) => {
        s"""${ns.anormParsers}.$name.parser($fieldName)"""
      }
      case ScalaPrimitive.Model(ns, name) => {
        val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
        s"${ns.anormParsers}.$name.parserWithPrefix($varName)"
      }
      case ScalaPrimitive.Union(ns, name) => {
        val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
        s"${ns.anormParsers}.$name.parserWithPrefix($varName)"
      }
    }
  }

  private[this] def generatePrimitiveRowParser(fieldName: String, datatype: ScalaPrimitive) = {
    s"SqlParser.get[${datatype.fullName}]($fieldName)"
  }

  private[this] def generateEnum(enum: ScalaEnum): String = {
    Seq(
      s"object ${enum.name} {",
      Seq(
        """def parserWithPrefix(prefix: String, sep: String = "_") = parser(s"""" + "$prefix${sep}name" + """")""",
        Seq(
          s"""def parser(name: String = "${enum.originalName}"): RowParser[${enum.qualifiedName}] = {""",
          s"  SqlParser.str(name) map {",
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

  private[this] def generateUnion(union: ScalaUnion): String = {
    Seq(
      s"object ${union.name} {",

      Seq(
        """def parserWithPrefix(prefix: String, sep: String = "_") = {""",
        union.types.map { t =>
          s"${t.ssd.namespaces.anormParsers}.${t.name}.parserWithPrefix(prefix, sep)"
        }.mkString(" |\n").indent(2),
        "}"
      ).mkString("\n").indent(2),

      Seq(
        "def parser() = {",
        union.types.map { t =>
          s"${t.ssd.namespaces.anormParsers}.${t.name}.parser()"
        }.mkString(" |\n").indent(2),
        "}"
      ).mkString("\n").indent(2),

      "}\n"
    ).mkString("\n\n")
  }

}
