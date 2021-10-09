package scala.generator.anorm

import scala.generator._
import scala.models.{ApiBuilderComments, Attributes, DateTimeTypeConfig, DateTypeConfig}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text
import lib.Text._

import scala.annotation.tailrec

object ParserGenerator24 extends ParserGenerator {
  override def attributes(ssd: ScalaService): ParserGeneratorPlayVersionSpecificAttributes = ParserGeneratorPlayVersionSpecificAttributes(
    imports = Seq(
      "anorm.{Column, MetaDataItem, TypeDoesNotMatch}",
      "play.api.libs.json.{JsArray, JsObject, JsValue}",
      "scala.util.{Failure, Success, Try}"
    )
  )
}

object ParserGenerator26 extends ParserGenerator {
  override def attributes(ssd: ScalaService): ParserGeneratorPlayVersionSpecificAttributes = {
    val dateImports = ssd.attributes.dateType match {
      case DateTypeConfig.JodaLocalDate => Seq(
        "play.api.libs.json.JodaReads._",
      )
      case _ => Nil
    }
    val dateTimeImports = ssd.attributes.dateTimeType match {
      case DateTimeTypeConfig.JodaDateTime => Seq(
        "play.api.libs.json.JodaReads._",
      )
      case _ => Nil
    }
    ParserGeneratorPlayVersionSpecificAttributes(
      imports = Seq(
        "anorm.{Column, MetaDataItem, TypeDoesNotMatch}",
        "play.api.libs.json.{JsArray, JsObject, JsValue}",
        "scala.util.{Failure, Success, Try}",
      ) ++ (dateImports ++ dateTimeImports).distinct
    )
  }
}

object ParserGenerator28 extends ParserGenerator {
  override def attributes(ssd: ScalaService): ParserGeneratorPlayVersionSpecificAttributes =
    ParserGenerator26.attributes(ssd)
}

case class ParserGeneratorPlayVersionSpecificAttributes(imports: Seq[String])

trait ParserGenerator extends CodeGenerator {

  def attributes(ssd: ScalaService): ParserGeneratorPlayVersionSpecificAttributes

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = new ScalaService(form.service, Attributes.PlayDefaultConfig.withAttributes(form.attributes))

    val header = ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n"

    Generator(ssd).code() match {
      case None => {
        Left(Seq("No enums, models, or unions were found and thus no parsers were generated"))
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
              header ++ Conversions.code(ssd, attributes(ssd)),
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

  private[this] case class Generator(ssd: ScalaService) {

    private[this] val requiredImports = scala.collection.mutable.Set[String]()
    addImports(ssd.namespaces)

    def code(): Option[String] = {
      Seq(ssd.models, ssd.enums, ssd.unions).flatten.toList match {
        case Nil => {
          None
        }
        case _ => {

          val clauses = Seq(
            ssd.enums.map(generateEnum),
            ssd.models.map(generateModel),
            ssd.unions.map(generateUnion)
          )

          Some(
            Seq(
              "import anorm._",
              s"package ${ssd.namespaces.anormParsers} {",
              s"import ${ssd.namespaces.anormConversions}.Standard._".indentString(2),
              requiredImports.toSeq.sorted.mkString("\n").indentString(2),
              clauses.flatten.map(_.trim).filter(_.nonEmpty).mkString("\n\n").indentString(2),
              "}"
            ).mkString("\n\n")
          )
        }
      }
    }

    private[this] def generateModel(model: ScalaModel): String = {
      Seq(
        s"object ${model.name} {",
        generateModelParser(model).indentString(2),
        "}\n"
      ).mkString("\n\n")
    }

    private[this] def generateModelParser(model: ScalaModel): String = {
      Seq(
        s"""def parserWithPrefix(prefix: String, sep: String = "_"): RowParser[${model.qualifiedName}] = parser(prefixOpt = Some(s"""" + "$prefix$sep" + """"))""",
        "",
        s"def parser(",
        (model.fields.map { f =>
          parserFieldDeclaration(f.name, f.datatype, f.originalName)
        } ++ List("prefixOpt: Option[String] = None")).mkString(",\n").indentString(2),
        s"): RowParser[${model.qualifiedName}] = {",
        Seq(
          model.fields.map { f => generateRowParser("""prefixOpt.getOrElse("") + """ + f.name, f.datatype, f.originalName) }.mkString(" ~\n") + " map {",
          Seq(
            "case " + model.fields.map(parserName).mkString(" ~ ") + " => {",
            Seq(
              s"${model.qualifiedName}(",
              model.fields.map { f =>
                s"${f.name} = ${parserName(f)}"
              }.mkString(",\n").indentString(2),
              ")"
            ).mkString("\n").indentString(2),
            "}"
          ).mkString("\n").indentString(2),
          "}"
        ).mkString("\n").indentString(2),
        "}"
      ).mkString("\n")
    }

    @scala.annotation.tailrec
    private[this] def parserFieldDeclaration(name: String, datatype: ScalaDatatype, originalName: String): String = {
      datatype match {
        case _ @ (ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | _: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | _: ScalaPrimitive.JsonObject | _: ScalaPrimitive.JsonValue | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid | _: ScalaPrimitive.Enum | _: ScalaDatatype.List | _: ScalaDatatype.Map) => {
          s"""$name: String = "$originalName""""
        }
        case ScalaPrimitive.Model(_, _) => {
          val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
          s"""$varName: String = "$originalName""""
        }
        case ScalaPrimitive.Union(_, _) => {
          val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
          s"""$varName: String = "$originalName""""
        }
        case ScalaDatatype.Option(inner) => {
          parserFieldDeclaration(name, inner, originalName)
        }
        case ScalaPrimitive.GeneratedModel(_) => sys.error("Generated models should not be persisted")
      }
    }

    private[this] def addImports(ns: Namespaces): Unit = {
      requiredImports += s"import ${ns.anormConversions}.Types._"
      ()
    }

    /**
      * Recursively adds anorm parser imports for any
      * datatype that is from an imported application.
      */
    @tailrec
    private[this] def addImports(datatype: ScalaDatatype): Unit = {
      datatype match {
        case _ @ (ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long | _: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | _: ScalaPrimitive.JsonObject | _: ScalaPrimitive.JsonValue | ScalaPrimitive.String | ScalaPrimitive.Unit | ScalaPrimitive.Uuid) => {
          // no-op
        }
        case ScalaDatatype.List(inner) => {
          addImports(inner)
        }
        case ScalaDatatype.Map(inner) => {
          addImports(inner)
        }
        case ScalaDatatype.Option(inner) => {
          addImports(inner)
        }
        case ScalaPrimitive.Enum(ns, _) => {
          addImports(ns)
        }
        case ScalaPrimitive.Model(ns, _) => {
          addImports(ns)
        }
        case ScalaPrimitive.Union(ns, _) => {
          addImports(ns)
        }
        case ScalaPrimitive.GeneratedModel(_) => sys.error("Generated models should not be persisted")
      }
    }

    private[this] def generateRowParser(fieldName: String, datatype: ScalaDatatype, originalName: String): String = {
      datatype match {
        case _ @ ScalaPrimitive.Boolean => s"SqlParser.bool($fieldName)"
        case f @ ScalaPrimitive.Double => generatePrimitiveRowParser(fieldName, f.asInstanceOf[ScalaPrimitive])
        case _ @ ScalaPrimitive.Integer => s"SqlParser.int($fieldName)"
        case _ @ ScalaPrimitive.Long => s"SqlParser.long($fieldName)"
        case f : ScalaPrimitive.DateIso8601 => generatePrimitiveRowParser(fieldName, f)
        case f : ScalaPrimitive.DateTimeIso8601 => generatePrimitiveRowParser(fieldName, f)
        case f @ ScalaPrimitive.Decimal => generatePrimitiveRowParser(fieldName, f.asInstanceOf[ScalaPrimitive])
        case f : ScalaPrimitive.JsonObject => generatePrimitiveRowParser(fieldName, f)
        case f : ScalaPrimitive.JsonValue => generatePrimitiveRowParser(fieldName, f)
        case _ @ ScalaPrimitive.String => s"SqlParser.str($fieldName)"
        case f @ ScalaPrimitive.Unit => generatePrimitiveRowParser(fieldName, f.asInstanceOf[ScalaPrimitive])
        case f @ ScalaPrimitive.Uuid => generatePrimitiveRowParser(fieldName, f.asInstanceOf[ScalaPrimitive])
        case _ @ ScalaDatatype.List(inner) => {
          addImports(inner)
          s"SqlParser.get[Seq[${inner.name}]]($fieldName)"
        }
        case _ @ ScalaDatatype.Map(inner) => {
          addImports(inner)
          s"SqlParser.get[Map[String, ${inner.name}]]($fieldName)"
        }
        case _ @ ScalaDatatype.Option(inner) => {
          addImports(inner)
          generateRowParser(fieldName, inner, originalName) + ".?"
        }
        case ScalaPrimitive.Enum(ns, name) => {
          addImports(ns)
          s"""${ns.anormParsers}.$name.parser($fieldName)"""
        }
        case ScalaPrimitive.Model(ns, name) => {
          addImports(ns)
          val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
          s"""${ns.anormParsers}.$name.parserWithPrefix(prefixOpt.getOrElse("") + $varName)"""
        }
        case ScalaPrimitive.Union(ns, name) => {
          addImports(ns)
          val varName = ScalaUtil.toVariable(s"${originalName}Prefix")
          s"""${ns.anormParsers}.$name.parserWithPrefix(prefixOpt.getOrElse("") + $varName)"""
        }
        case ScalaPrimitive.GeneratedModel(_) => sys.error("Generated models should not be persisted")
      }
    }

    private[this] def generatePrimitiveRowParser(fieldName: String, datatype: ScalaPrimitive) = {
      s"SqlParser.get[${datatype.fullName}]($fieldName)"
    }

    private[this] def generateEnum(enum: ScalaEnum): String = {
      Seq(
        s"object ${enum.name} {",
        Seq(
          s"""def parserWithPrefix(prefix: String, sep: String = "_"): RowParser[${enum.qualifiedName}] = parser(prefixOpt = Some(s"""" + "$prefix$sep" + """"))""",
          Seq(
            s"""def parser(name: String = "${enum.originalName}", prefixOpt: Option[String] = None): RowParser[${enum.qualifiedName}] = {""",
            s"""  SqlParser.str(prefixOpt.getOrElse("") + name) map {""",
            s"    case value => ${enum.qualifiedName}(value)",
            s"  }",
            s"}"
          ).mkString("\n")
        ).mkString("\n\n").indentString(2),
        "}"
      ).mkString("\n\n")
    }

    /**
      * Parsers require special quoting of keywords - if the field name
      * is a keyword, we append Instance (back ticks will not compile)
      */
    private[this] def parserName(field: ScalaField): String = {
      Text.initLowerCase(if (ScalaUtil.isKeyword(field.originalName)) {
        Text.snakeToCamelCase(field.originalName) + "Instance"
      } else {
        field.name
      })
    }

    private[this] def generateUnion(union: ScalaUnion): String = {
      import PrimitiveWrapper._
      Seq(
        s"object ${union.name} {",

        Seq(
          """def parserWithPrefix(prefix: String, sep: String = "_") = {""",
          union.types.map { t =>
            t.datatype match {
              case _: ScalaPrimitive.Enum => s"""${t.ssd.namespaces.anormParsers}.${t.name}.parser("${union.originalName}", Some(s"""" + "$prefix$sep" + """"))"""
              case p: ScalaPrimitive if isBasicType(p) => generateRowParser("""s"$prefix${sep}""" + s"""${union.originalName}"""", t.datatype, union.originalName) + s""".map(${t.ssd.namespaces.models}.${className(union, p)}.apply)"""
              case _ => s"""${t.ssd.namespaces.anormParsers}.${t.name}.parser(prefixOpt = Some(s"""" + "$prefix$sep" + """"))"""
            }
          }.mkString(" |\n").indentString(2),
          "}"
        ).mkString("\n").indentString(2),

        Seq(
          "def parser() = {",
          union.types.map { t =>
            t.datatype match {
              case _: ScalaPrimitive.Enum => s"""${t.ssd.namespaces.anormParsers}.${t.name}.parser("${union.originalName}")"""
              case p: ScalaPrimitive if isBasicType(p) => generateRowParser(s""""${union.originalName}"""", t.datatype, union.originalName) + s""".map(${t.ssd.namespaces.models}.${className(union, p)}.apply)"""
              case _ => s"""${t.ssd.namespaces.anormParsers}.${t.name}.parser()"""
            }
          }.mkString(" |\n").indentString(2),
          "}"
        ).mkString("\n").indentString(2),

        "}\n"
      ).mkString("\n\n")
    }

  }

}
