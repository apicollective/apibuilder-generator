package models

import com.gilt.apidocgenerator.models.{InvocationForm, Service}
import lib.Text._
import generator.{ScalaEnums, ScalaCaseClasses, ScalaService, CodeGenerator}

object Play2Models extends CodeGenerator {

  override def invoke(form: InvocationForm): String = {
    apply(form)
  }

  def apply(
    form: InvocationForm,
    addHeader: Boolean = true
  ): String = {
    val ssd = ScalaService(form.service)

    val caseClasses = ScalaCaseClasses.invoke(form, addHeader = false)
    val prefix = underscoreAndDashToInitCap(ssd.name)
    val enumJson: String = ssd.enums.map { ScalaEnums(_).buildJson(ssd.name) }.mkString("\n\n")
    val modelJson: String = ssd.models.map { Play2Json(ssd.name, _).generate() }.mkString("\n\n")

    val header = addHeader match {
      case false => ""
      case true => ApidocHeaders(form.userAgent).toJavaString() + "\n"
    }

s"""$header$caseClasses

package ${ssd.modelPackageName} {
  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._

    private[${ssd.packageNamePrivate}] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

    private[${ssd.packageNamePrivate}] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
      def writes(x: java.util.UUID) = JsString(x.toString)
    }

    private[${ssd.packageNamePrivate}] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
      import org.joda.time.format.ISODateTimeFormat.dateTimeParser
      dateTimeParser.parseDateTime(str)
    }

    private[${ssd.packageNamePrivate}] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
      def writes(x: org.joda.time.DateTime) = {
        import org.joda.time.format.ISODateTimeFormat.dateTime
        val str = dateTime.print(x)
        JsString(str)
      }
    }

${enumJson.indent(4)}
${modelJson.indent(4)}
  }
}"""
  }
}
