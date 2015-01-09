package models

import lib.Text
import generator.{ScalaEnum, ScalaService}

object Play2Bindables {

  def build(
    ssd: ScalaService
  ): String = {
    import lib.Text._

    Seq(
      "object Bindables {",
      "",
      "  import play.api.mvc.{PathBindable, QueryStringBindable}",
      "  import org.joda.time.{DateTime, LocalDate}",
      "  import org.joda.time.format.ISODateTimeFormat",
      s"  import ${ssd.namespace}.models._",
      "",
      buildDefaults().indent(2),
      "",
      ssd.enums.map { e => buildImplicit(e.name) }.mkString("\n\n").indent(2),
      "",
      "}"
    ).mkString("\n")
  }

  private def buildDefaults(): String = {
    """
// Type: date-time-iso8601
implicit val pathBindableTypeDateTimeIso8601 = new PathBindable.Parsing[DateTime](
  ISODateTimeFormat.dateTimeParser.parseDateTime(_), _.toString, (key: String, e: Exception) => s"Error parsing date time $key. Example: 2014-04-29T11:56:52Z"
)

implicit val queryStringBindableTypeDateTimeIso8601 = new QueryStringBindable.Parsing[DateTime](
  ISODateTimeFormat.dateTimeParser.parseDateTime(_), _.toString, (key: String, e: Exception) => s"Error parsing date time $key. Example: 2014-04-29T11:56:52Z"
)

// Type: date-iso8601
implicit val pathBindableTypeDateIso8601 = new PathBindable.Parsing[LocalDate](
  ISODateTimeFormat.yearMonthDay.parseLocalDate(_), _.toString, (key: String, e: Exception) => s"Error parsing date $key. Example: 2014-04-29"
)

implicit val queryStringBindableTypeDateIso8601 = new QueryStringBindable.Parsing[LocalDate](
  ISODateTimeFormat.yearMonthDay.parseLocalDate(_), _.toString, (key: String, e: Exception) => s"Error parsing date $key. Example: 2014-04-29"
)
""".trim
  }

  private[models] def buildImplicit(
    enumName: String
  ): String = {
    s"// Enum: $enumName\n" +
    """private val enum%sNotFound = (key: String, e: Exception) => s"Unrecognized $key, should be one of ${%s.all.mkString(", ")}"""".format(enumName, enumName) +
    s"""

implicit val pathBindableEnum$enumName = new PathBindable.Parsing[$enumName] (
  $enumName.fromString(_).get, _.toString, enum${enumName}NotFound
)

implicit val queryStringBindableEnum$enumName = new QueryStringBindable.Parsing[$enumName](
  $enumName.fromString(_).get, _.toString, enum${enumName}NotFound
)"""
  }

}
