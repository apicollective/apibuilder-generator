package scala.models.play.files

import org.scalatest.{FunSpec, Matchers}
import scala.models.play.Helpers.compareWithoutWhiteSpaces

class ModelsJsonSpec extends FunSpec with Matchers {
  it("generates reader, and writer for uuid") {
    val expected = s"""
      import play.api.libs.json.Reads.uuidReads
      import play.api.libs.json.Writes.UuidWrites
    """

    val result = ModelsJson.uuidFormat()
    compareWithoutWhiteSpaces(expected, result)
  }

  it("generates reader, and writer for joda DateTime") {
    val expected = s"""
      import play.api.libs.json.JodaReads.DefaultJodaDateTimeReads
      import play.api.libs.json.JodaWrites.JodaDateTimeWrites
    """

    val result = ModelsJson.jodaDateTimeFormat()
    compareWithoutWhiteSpaces(expected, result)
  }

  it("generates reader, and writer for joda LocalDate") {
    val expected = s"""
      import play.api.libs.json.JodaReads.DefaultJodaLocalDateReads
      import play.api.libs.json.JodaWrites.DefaultJodaLocalDateWrites
    """

    val result = ModelsJson.jodaLocalDateFormat()
    compareWithoutWhiteSpaces(expected, result)
  }
}
