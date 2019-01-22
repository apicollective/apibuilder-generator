package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator
import scala.generator.{ScalaCaseClasses, ScalaService}
import generator.ServiceFileNames

object Play2Models extends Play2Models

trait Play2Models extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    Right(generateCode(form = form, addBindables = true, addHeader = true))
  }

  def generateCode(
    form: InvocationForm,
    addBindables: Boolean,
    addHeader: Boolean
  ): Seq[File] = {
    val ssd = ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))

    val caseClasses = ScalaCaseClasses.generateCode(ssd, form.userAgent, addHeader = false).map(_.contents).mkString("\n\n")
    val prefix = underscoreAndDashToInitCap(ssd.name)
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

    val timeSerde = ssd.config.timeLib match {
      case TimeConfig.JodaTime => s"""
        |private[${ssd.namespaces.last}] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
        |  import org.joda.time.format.ISODateTimeFormat.dateTimeParser
        |  dateTimeParser.parseDateTime(str)
        |}
        |
        |private[${ssd.namespaces.last}] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
        |  def writes(x: org.joda.time.DateTime) = {
        |    import org.joda.time.format.ISODateTimeFormat.dateTime
        |    val str = dateTime.print(x)
        |    JsString(str)
        |  }
        |}
        |
        |private[${ssd.namespaces.last}] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
        |  import org.joda.time.format.ISODateTimeFormat.dateParser
        |  dateParser.parseLocalDate(str)
        |}
        |
        |private[${ssd.namespaces.last}] implicit val jsonWritesJodaLocalDate = new Writes[org.joda.time.LocalDate] {
        |  def writes(x: org.joda.time.LocalDate) = {
        |    import org.joda.time.format.ISODateTimeFormat.date
        |    val str = date.print(x)
        |    JsString(str)
        |  }
        |}""".stripMargin
      case TimeConfig.JavaTime => s"""
        |private[${ssd.namespaces.last}] implicit val jsonReadsJavaInstant = __.read[String].map { str =>
        |  _root_.java.time.Instant.parse(str)
        |}
        |
        |private[${ssd.namespaces.last}] implicit val jsonWritesJavaInstant = new Writes[_root_.java.time.Instant] {
        |  def writes(x: _root_.java.time.Instant) = {
        |    JsString(x.toString)
        |  }
        |}
        |
        |private[${ssd.namespaces.last}] implicit val jsonReadsJavaLocalDate = __.read[String].map { str =>
        |  _root_.java.time.LocalDate.parse(str)
        |}
        |
        |private[${ssd.namespaces.last}] implicit val jsonWritesJavaLocalDate = new Writes[_root_.java.time.LocalDate] {
        |  def writes(x: _root_.java.time.LocalDate) = {
        |    JsString(x.toString)
        |  }
        |}""".stripMargin
    }

    val source = s"""$header$caseClasses

package ${ssd.namespaces.models} {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
${JsonImports(form.service).mkString("\n").indent(4)}

    private[${ssd.namespaces.last}] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

    private[${ssd.namespaces.last}] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
      def writes(x: java.util.UUID) = JsString(x.toString)
    }
${timeSerde.indent(4)}

${Seq(enumJson, modelAndUnionJson).filter(!_.isEmpty).mkString("\n\n").indent(4)}
  }
}
$bindables
"""

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Models", source, Some("Scala")))
  }

}
