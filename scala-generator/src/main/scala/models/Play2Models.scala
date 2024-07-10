package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator

import scala.generator.{ScalaCaseClasses, ScalaService}
import generator.ServiceFileNames

import scala.generator.ScalaPrimitive.{DateIso8601Joda, DateTimeIso8601Joda, Uuid}

object Play2ModelsScala2 extends Play2Models(ScalaVersion(2))
object Play2ModelsScala3 extends Play2Models(ScalaVersion(3))

object Play2ModelImplicits {
  val play: String = {
    s"""import play.api.libs.json.Writes._
       |import play.api.libs.json.Reads._""".stripMargin

  }

  val jodaDateTime: String = {
    s"""import play.api.libs.json.JodaReads.DefaultJodaDateTimeReads
       |import play.api.libs.json.JodaWrites.JodaDateTimeWrites""".stripMargin
  }

  val jodaLocalDate: String = {
    s"""import play.api.libs.json.JodaReads.DefaultJodaLocalDateReads
       |import play.api.libs.json.JodaWrites.DefaultJodaLocalDateWrites""".stripMargin
  }
}

case class Play2Models(version: ScalaVersion) extends CodeGenerator {
  private[this] val scala3Support: Boolean = version.major >= 3

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
    val play2json = Play2Json(ssd, scala3Support)
    val enumJson: String = play2json.generateEnums()
    val modelAndUnionJson: String = play2json.generateModelsAndUnions()

    val header = if (addHeader) {
      ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n"
    } else {
      ""
    }

    val bindables = if (addBindables) {
      "\n" +
        Seq(
          s"package ${ssd.namespaces.base} {",
          Play2Bindables(ssd).build().indentString(2),
          "}"
        ).mkString("\n\n")
    } else {
      ""
    }

    val serDes = if (useBuiltInImplicits) {
      Seq(Play2ModelImplicits.play) ++
      (ssd.attributes.dateTimeType match {
        case DateTimeTypeConfig.JodaDateTime => Seq(Play2ModelImplicits.jodaDateTime)
        case _ => Nil
      }) ++
      (ssd.attributes.dateType match {
        case DateTypeConfig.JodaLocalDate => Seq(Play2ModelImplicits.jodaLocalDate)
        case _ => Nil
      })
    } else {
      (ssd.attributes.dateTimeType match {
        case DateTimeTypeConfig.JodaDateTime => Seq(manualImplicits(ssd))
        case _ => Seq(Play2ModelImplicits.play)
      }) ++
      (ssd.attributes.dateType match {
        case DateTypeConfig.JodaLocalDate => Seq(manualImplicits(ssd))
        case _ => Seq(Play2ModelImplicits.play)
      })
    }

    val source = s"""$header$caseClasses

package ${ssd.namespaces.models} {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
${JsonImports(form.service).mkString("\n").indentString(4)}
${serDes.distinct.mkString("\n").indentString(4)}

${Seq(enumJson, modelAndUnionJson).filter(_.nonEmpty).mkString("\n\n").indentString(4)}
  }
}
$bindables
"""

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Models", source, Some("Scala")))
  }

  def manualImplicits(ssd: ScalaService): String = {
    s"""
       |private[${ssd.namespaces.last}] implicit val jsonReadsUUID: play.api.libs.json.Reads[_root_.java.util.UUID] = __.read[String].map { str =>
       |  ${Uuid.fromStringValue("str")}
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonWritesUUID: play.api.libs.json.Writes[_root_.java.util.UUID] = (x: _root_.java.util.UUID) => play.api.libs.json.JsString(x.toString)
       |
       |private[${ssd.namespaces.last}] implicit val jsonReadsJodaDateTime: play.api.libs.json.Reads[${DateTimeIso8601Joda.fullName}] = __.read[String].map { str =>
       |  ${DateTimeIso8601Joda.fromStringValue("str")}
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonWritesJodaDateTime: play.api.libs.json.Writes[${DateTimeIso8601Joda.fullName}] = (x: _root_.org.joda.time.DateTime) => {
       |  play.api.libs.json.JsString(${DateTimeIso8601Joda.asString("x")})
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonReadsJodaLocalDate: play.api.libs.json.Reads[${DateIso8601Joda.fullName}] = __.read[String].map { str =>
       |  ${DateIso8601Joda.fromStringValue("str")}
       |}
       |
       |private[${ssd.namespaces.last}] implicit val jsonWritesJodaLocalDate: play.api.libs.json.Writes[${DateIso8601Joda.fullName}] = (x: _root_.org.joda.time.LocalDate) => {
       |  play.api.libs.json.JsString(${DateIso8601Joda.asString("x")})
       |}""".stripMargin
  }
}
