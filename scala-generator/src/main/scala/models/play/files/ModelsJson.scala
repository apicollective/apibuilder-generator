package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm
import scala.generator.Namespaces

object ModelsJson {

    def uuidFormat(namespaces: Namespaces): String = s"""
        private[${namespaces.last}] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

        private[${namespaces.last}] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
            def writes(x: java.util.UUID) = JsString(x.toString)
        }
    """

    def jodaDateTimeFormat(namespaces: Namespaces): String = s"""
        private[${namespaces.last}] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
            import org.joda.time.format.ISODateTimeFormat.dateTimeParser
            dateTimeParser.parseDateTime(str)
        }

        private[${namespaces.last}] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
            def writes(x: org.joda.time.DateTime) = {
                import org.joda.time.format.ISODateTimeFormat.dateTime
                val str = dateTime.print(x)
                JsString(str)
            }
        }
    """

    def jodaLocalDateFormat(namespaces: Namespaces): String = s"""
        private[${namespaces.last}] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
            import org.joda.time.format.ISODateTimeFormat.dateParser
            dateParser.parseLocalDate(str)
        }

        private[${namespaces.last}] implicit val jsonWritesJodaLocalDate = new Writes[org.joda.time.LocalDate] {
            def writes(x: org.joda.time.LocalDate) = {
                import org.joda.time.format.ISODateTimeFormat.date
                val str = date.print(x)
                JsString(str)
            }
        }
    """

    def contents(form: InvocationForm): String = {
        val scalaService = scala.generator.ScalaService(form.service)

        val header = scala.models.ApidocComments(form.service.version, form.userAgent)
        val imports = scala.models.JsonImports(form.service)

        val gen = scala.models.Play2Json(scalaService)

        s"""
            ${header.toJavaString()}

            package ${scalaService.namespaces.models}

            package object json {

                import play.api.libs.json.{__, JsString, Writes}
                import play.api.libs.functional.syntax._

                ${imports.mkString("\n")}

                ${uuidFormat(scalaService.namespaces)}
                ${jodaDateTimeFormat(scalaService.namespaces)}
                ${jodaLocalDateFormat(scalaService.namespaces)}

                ${gen.generateEnums()}
                ${gen.generateModelsAndUnions()}

            }
        """
    }
}
