package models

import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.Service
import lib.Text._
import generator.{PrimitiveWrapper, ScalaEnums, ScalaCaseClasses, ScalaService, CodeGenerator}

object Play2Models extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], String] = {
    ScalaCaseClasses.modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(generateCode(form = form, addBindables = true, addHeader = true))
      case errors => Left(errors)
    }
  }

  def generateCode(
    form: InvocationForm,
    addBindables: Boolean,
    addHeader: Boolean
  ): String = {
    val ssd = ScalaService(form.service)

    val caseClasses = ScalaCaseClasses.generateCode(form, addHeader = false)
    val prefix = underscoreAndDashToInitCap(ssd.name)
    val enumJson: String = ssd.enums.map { ScalaEnums(ssd, _).buildJson() }.mkString("\n\n")
    val play2Json = Play2Json(ssd).generate()

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

    s"""$header$caseClasses

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

    private[${ssd.namespaces.last}] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
      import org.joda.time.format.ISODateTimeFormat.dateTimeParser
      dateTimeParser.parseDateTime(str)
    }

    private[${ssd.namespaces.last}] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
      def writes(x: org.joda.time.DateTime) = {
        import org.joda.time.format.ISODateTimeFormat.dateTime
        val str = dateTime.print(x)
        JsString(str)
      }
    }

${Seq(enumJson, play2Json).filter(!_.isEmpty).mkString("\n\n").indent(4)}
  }
}
$bindables
"""
  }
}
