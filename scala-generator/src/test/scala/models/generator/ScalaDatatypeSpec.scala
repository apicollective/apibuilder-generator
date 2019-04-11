package scala.generator

import scala.annotation.tailrec

import ScalaPrimitive._
import ScalaDatatype._

import org.scalatest.{FunSpec, Matchers}

class ScalaDatatypeSpec extends FunSpec with Matchers {

  it("should generate the right variable name when nested") {
    val model = new Model(Namespaces("org.example"), "User")
    val string = ScalaPrimitive.String

    @tailrec
    def nest[T <: ScalaDatatype](
      d: ScalaDatatype, nester: ScalaDatatype => T, levels: Int
    ): ScalaDatatype = {
      if (levels <= 0) d else nest(nester(d), nester, levels - 1)
    }

    def nestList(d: ScalaDatatype, levels: Int): ScalaDatatype = {
      nest(d, List(_), levels)
    }

    def nestMap(d: ScalaDatatype, levels: Int): ScalaDatatype = {
      nest(d, Map(_), levels)
    }

    nestList(model, 0).toVariableName should be("user")
    nestList(model, 1).toVariableName should be("users")
    nestList(model, 2).toVariableName should be("users")
    nestList(model, 3).toVariableName should be("users")

    nestMap(model, 0).toVariableName should be("user")
    nestMap(model, 1).toVariableName should be("users")
    nestMap(model, 2).toVariableName should be("users")
    nestMap(model, 3).toVariableName should be("users")

    nestList(string, 0).toVariableName should be("value")
    nestList(string, 1).toVariableName should be("values")
    nestList(string, 2).toVariableName should be("values")
    nestList(string, 3).toVariableName should be("values")

    nestMap(string, 0).toVariableName should be("value")
    nestMap(string, 1).toVariableName should be("values")
    nestMap(string, 2).toVariableName should be("values")
    nestMap(string, 3).toVariableName should be("values")
  }

  it("DateIso8601Joda sanity check") {
    DateIso8601Joda.asString("myVar") shouldBe "myVar.toString"
    DateIso8601Joda.default("2020-12-31") shouldBe "new _root_.org.joda.time.LocalDate(2020, 12, 31)"
    DateIso8601Joda.name shouldBe "_root_.org.joda.time.LocalDate"
  }

  it("DateIso8601Java sanity check") {
    DateIso8601Java.asString("myVar") shouldBe "myVar.toString"
    DateIso8601Java.default("2020-12-31") shouldBe "_root_.java.time.LocalDate.parse(\"2020-12-31\")"
    DateIso8601Java.name shouldBe "_root_.java.time.LocalDate"
  }

  it("DateTimeIso8601Joda sanity check") {
    DateTimeIso8601Joda.asString("myVar") shouldBe "_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(myVar)"
    DateTimeIso8601Joda.default("2020-12-31") shouldBe "_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(\"2020-12-31\")"
    DateTimeIso8601Joda.name shouldBe "_root_.org.joda.time.DateTime"
  }

  it("DateTimeIso8601Java sanity check") {
    DateTimeIso8601Java.asString("myVar") shouldBe "myVar.toString"
    DateTimeIso8601Java.default("2020-12-31") shouldBe "_root_.java.time.OffsetDateTime.parse(\"2020-12-31\").toInstant"
    DateTimeIso8601Java.name shouldBe "_root_.java.time.Instant"
  }
}


