package scala.models.play.files

import org.scalatest.{FunSpec, Matchers}
import scala.models.play.Helpers.compareWithoutWhiteSpaces

class ModelsJsonSpec extends FunSpec with Matchers {
    it("generates reader, and writer for uuid") {
        val namespaces = scala.generator.Namespaces("john.doe")
        val expected = s"""
            private[${namespaces.last}] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

            private[${namespaces.last}] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
                def writes(x: java.util.UUID) = JsString(x.toString)
            }
        """

        val result = ModelsJson.uuidFormat(namespaces)

        compareWithoutWhiteSpaces(expected, result)
    }

    it("generates reader, and writer for joda DateTime") {
        val namespaces = scala.generator.Namespaces("john.doe")
        val expected = s"""
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

        val result = ModelsJson.jodaDateTimeFormat(namespaces)

        compareWithoutWhiteSpaces(expected, result)
    }

    it("generates reader, and writer for joda LocalDate") {
        val namespaces = scala.generator.Namespaces("john.doe")
        val expected = s"""
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

        val result = ModelsJson.jodaLocalDateFormat(namespaces)

        compareWithoutWhiteSpaces(expected, result)
    }
}
