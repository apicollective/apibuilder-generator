package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator

import scala.generator.{ScalaCaseClasses, ScalaService}
import generator.ServiceFileNames

import scala.generator.ScalaPrimitive.{DateIso8601Joda, DateTimeIso8601Joda, Uuid}

object Play2Models extends Play2Models

trait Play2Models extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    Right(generateCode(form = form, addBindables = true, addHeader = true, useBuiltInImplicits = false))
  }

  def generateCode(
    form: InvocationForm,
    addBindables: Boolean,
    addHeader: Boolean,
    useBuiltInImplicits: Boolean,
  ): Seq[File] = {
    val ssd = ScalaService(form.service, Attributes.PlayDefaultConfig.withAttributes(form.attributes))

    val caseClasses = ScalaCaseClasses.generateCode(ssd, form.userAgent, addHeader = false).map(_.contents).mkString("\n\n")
    val play2json = Play2Json(ssd)
    val enumJson: String = play2json.generateEnums()
    val modelAndUnionJson: String = play2json.generateModelsAndUnions()

    val header = addHeader match {
      case false => ""
      case true => ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
    }

    val bindables = addBindables match {
      case false => ""
      case true => {
        "\n" +
        Seq(
          s"package ${ssd.namespaces.base} {",
          Play2Bindables(ssd).build().indent(2),
          "}"
        ).mkString("\n\n")
      }
    }

    val serDes = if (useBuiltInImplicits) {
      Seq(timeImplicits) ++
      (ssd.attributes.dateTimeType match {
        case DateTimeTypeConfig.JodaDateTime => Seq(jodaDateTimeImplicits)
        case _ => Nil
      }) ++
      (ssd.attributes.dateType match {
        case DateTypeConfig.JodaLocalDate => Seq(jodaLocalDateImplicits)
        case _ => Nil
      })
    } else {
      (ssd.attributes.dateTimeType match {
        case DateTimeTypeConfig.JodaDateTime => Seq(manualImplicits(ssd))
        case _ => Seq(timeImplicits)
      }) ++
      (ssd.attributes.dateType match {
        case DateTypeConfig.JodaLocalDate => Seq(manualImplicits(ssd))
        case _ => Seq(timeImplicits)
      })
    }

    val source = s"""$header$caseClasses

package ${ssd.namespaces.models} {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
${JsonImports(form.service).mkString("\n").indent(4)}
${serDes.distinct.mkString("\n").indent(4)}

${Seq(enumJson, modelAndUnionJson).filter(!_.isEmpty).mkString("\n\n").indent(4)}
  }
}
$bindables
"""

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Models", source, Some("Scala")))
  }

  val timeImplicits = {
    s"""import play.api.libs.json.Writes._
       |import play.api.libs.json.Reads._""".stripMargin

  }

  val jodaDateTimeImplicits = {
    s"""import play.api.libs.json.JodaReads.DefaultJodaDateTimeReads
       |import play.api.libs.json.JodaWrites.JodaDateTimeWrites""".stripMargin
  }

  val jodaLocalDateImplicits = {
    s"""import play.api.libs.json.JodaReads.DefaultJodaLocalDateReads
       |import play.api.libs.json.JodaWrites.DefaultJodaLocalDateWrites""".stripMargin
  }

  def manualImplicits(ssd: ScalaService) = {
    s"""
       |private[${ssd.namespaces.last}] implicit val jsonReadsUUID = __.read[String].map { str =>
       |  ${Uuid.fromStringValue("str")}
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonWritesUUID = new Writes[${Uuid.fullName}] {
       |  def writes(x: ${Uuid.fullName}) = JsString(${Uuid.asString("x")})
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
       |  ${DateTimeIso8601Joda.fromStringValue("str")}
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonWritesJodaDateTime = new Writes[${DateTimeIso8601Joda.fullName}] {
       |  def writes(x: ${DateTimeIso8601Joda.fullName}) = {
       |    JsString(${DateTimeIso8601Joda.asString("x")})
       |  }
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
       |  ${DateIso8601Joda.fromStringValue("str")}
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonWritesJodaLocalDate = new Writes[${DateIso8601Joda.fullName}] {
       |  def writes(x: ${DateIso8601Joda.fullName}) = {
       |    JsString(${DateIso8601Joda.asString("x")})
       |  }
       |}""".stripMargin
  }
}
