package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator._
import scala.models.play.Helpers._

class JsonFormats(service: ScalaService) extends Component {

  def uuid(packageName: String): String = s"""
    |private[${packageName}] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)
    |private[${packageName}] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
    |  def writes(x: java.util.UUID) = JsString(x.toString)
    |}
  """

  def dateTime(packageName: String): String = s"""
    |private[${packageName}] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
    |  import org.joda.time.format.ISODateTimeFormat.dateTimeParser
    |  dateTimeParser.parseDateTime(str)
    |}
    |
    |private[${packageName}] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
    |  def writes(x: org.joda.time.DateTime) = {
    |    import org.joda.time.format.ISODateTimeFormat.dateTime
    |    val str = dateTime.print(x)
    |    JsString(str)
    |  }
    |}
  """

  def localDate(packageName: String): String = s"""
    |private[${packageName}] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
    |  import org.joda.time.format.ISODateTimeFormat.dateParser
    |  dateParser.parseLocalDate(str)
    |}
    |
    |private[${packageName}] implicit val jsonWritesJodaLocalDate = new Writes[org.joda.time.LocalDate] {
    |  def writes(x: org.joda.time.LocalDate) = {
    |    import org.joda.time.format.ISODateTimeFormat.date
    |    val str = date.print(x)
    |    JsString(str)
    |  }
    |}
  """

  def jsonImports(service: ScalaService): String = {
    (service.namespaces.base +: service.service.imports.map(_.namespace))
      .map { p => s"import ${p}.json._" }
      .mkString("\n")
  }

  def code(): ValidatedNel[String, String] = {
    val gen = scala.models.Play2Json(service)
    val code = s"""
      |package object json {
      |  import play.api.libs.json.{__, JsString, Writes}
      |  import play.api.libs.functional.syntax._
      |
      |  import ${service.namespaces.base}.models._
      |  ${jsonImports(service).addMargin(2)}
      |
      |  ${uuid(service.namespaces.last).addMargin(2)}
      |  ${dateTime(service.namespaces.last).addMargin(2)}
      |  ${localDate(service.namespaces.last).addMargin(2)}
      |
      |  ${gen.generateEnums().addMargin(2)}
      |
      |  ${gen.generateModelsAndUnions().addMargin(2)}
      |}
    """.replaceAll(".models.json.", ".json.")

    Validated.validNel(code)
  }
}
