package scala.generator

import org.scalatest.{ Matchers, FunSpec }

class ScalaUtilSpec extends FunSpec with Matchers {

  it("extendsClause") {
    ScalaUtil.extendsClause(Nil) should be(None)
    ScalaUtil.extendsClause(Seq("Foo")).get should be(" extends Foo")
    ScalaUtil.extendsClause(Seq("Foo", "Bar")).get should be(" extends Bar with Foo")
    ScalaUtil.extendsClause(Seq("Foo", "Bar", "Baz")).get should be(" extends Bar with Baz with Foo")
  }

  it("toClassName") {
    ScalaUtil.toClassName("UnableToFulfill") should be("UnableToFulfill")
    ScalaUtil.toClassName("UNABLE_TO_FULFILL") should be("UnableToFulfill")

    ScalaUtil.toClassName("error") should be("Error")

    ScalaUtil.toClassName("error_message") should be("ErrorMessage")

    ScalaUtil.toClassName("incidents_create") should be("IncidentsCreate")
    ScalaUtil.toClassName("incidents-create") should be("IncidentsCreate")
    ScalaUtil.toClassName("incidents.create") should be("IncidentsCreate")
    ScalaUtil.toClassName("incident.create") should be("IncidentCreate")
    ScalaUtil.toClassName("incidents:create") should be("IncidentsCreate")
    ScalaUtil.toClassName("incident:create") should be("IncidentCreate")
    ScalaUtil.toClassName("incidents/create") should be("IncidentsCreate")
  }

  it("toVariable") {
    ScalaUtil.toVariable("Foo") should be("foo")
    ScalaUtil.toVariable("FooBar") should be("fooBar")
    ScalaUtil.toVariable("Foo_Bar") should be("fooBar")
    ScalaUtil.toVariable("foo_bar") should be("fooBar")

    ScalaUtil.toVariable("error") should be("error")
    ScalaUtil.toVariable("errors") should be("errors")
    ScalaUtil.toVariable("error_message") should be("errorMessage")
    ScalaUtil.toVariable("error_messages") should be("errorMessages")
  }

  it("quoteNameIfKeyword") {
    ScalaUtil.quoteNameIfKeyword("foo") should be("foo")
    ScalaUtil.quoteNameIfKeyword("val") should be("`val`")
  }

  describe("textToComment") {
    it("For single line comment") {
      ScalaUtil.textToComment("users is a") should be("""/**
 * users is a
 */
""".trim)
    }

    it("is a no op for whitespace") {
      ScalaUtil.textToComment("") should be("")
      ScalaUtil.textToComment("  ") should be("")
    }

    it("Breaks up a long comment") {
      val source = "Search all users. Results are always paginated. You must specify at least 1 parameter"
      val target = """/**
 * Search all users. Results are always paginated. You must specify at least 1
 * parameter
 */
""".trim

      ScalaUtil.textToComment(source) should be(target)
    }

    it("keeps newlines in the input") {
      val source =
        """|Method desc goes here
           |@param x the first argument
           |@param y the second argument""".stripMargin


      ScalaUtil.textToComment(source) should be(
        """|/**
           | * Method desc goes here
           | * @param x the first argument
           | * @param y the second argument
           | */""".stripMargin)
    }

    it("avoids empty lines") {
      ScalaUtil.textToComment(
        Seq(
          "description",
          " ",
        )
      ) should be(
        """
          |/**
          | * description
          | *
          | */
          | """.stripMargin.trim
      )
    }
  }

  describe("scalaDefault") {
    it ("should fail for datatype: object") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.ObjectAsPlay)
      }
      ex.getMessage should be("parsing default `` for datatype ObjectAsPlay")
    }

    it ("should fail for datatype: json") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.JsonValueAsPlay)
      }
      ex.getMessage should be("parsing default `` for datatype JsonValueAsPlay")
    }

    it ("should fail for datatype: unit") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Unit)
      }
      ex.getMessage should be("parsing default `` for datatype Unit")
    }

    it ("default: foo, datatype: enum") {
      ScalaUtil.scalaDefault("foo", ScalaPrimitive.Enum(Namespaces("test"), "test")) should be("test.models.test.Foo")
    }

    it ("should fail for datatype: model") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Model(Namespaces("test"), "test"))
      }
      ex.getMessage should be("parsing default `` for datatype Model(Namespaces(test),test)")
    }

    it ("should fail for datatype: union") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Union(Namespaces("test"), "test"))
      }
      ex.getMessage should be("parsing default `` for datatype Union(Namespaces(test),test)")
    }

    it ("should fail for datatype: option") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaDatatype.Option(ScalaPrimitive.Double))
      }
      ex.getMessage should be("parsing default `` for datatype Option(Double)")
    }

    it ("default: true, datatype: boolean") {
      ScalaUtil.scalaDefault("true", ScalaPrimitive.Boolean) should be("true")
    }

    it ("default: false, datatype: boolean") {
      ScalaUtil.scalaDefault("false", ScalaPrimitive.Boolean) should be("false")
    }

    it ("default: 1234.5, datatype: double") {
      ScalaUtil.scalaDefault("1234.5", ScalaPrimitive.Double) should be("1234.5")
    }

    it ("default: -1234.5, datatype: double") {
      ScalaUtil.scalaDefault("-1234.5", ScalaPrimitive.Double) should be("-1234.5")
    }

    it ("default: 123, datatype: double") {
      ScalaUtil.scalaDefault("123", ScalaPrimitive.Double) should be("123.0")
    }

    it ("default: 124, datatype: integer") {
      ScalaUtil.scalaDefault("124", ScalaPrimitive.Integer) should be("124")
    }

    it ("default: -124, datatype: integer") {
      ScalaUtil.scalaDefault("-124", ScalaPrimitive.Integer) should be("-124")
    }

    it ("default: 00, datatype: integer") {
      ScalaUtil.scalaDefault("00", ScalaPrimitive.Integer) should be("0")
    }

    it ("default: 124, datatype: long") {
      ScalaUtil.scalaDefault("124", ScalaPrimitive.Long) should be("124L")
    }

    it ("default: -124, datatype: long") {
      ScalaUtil.scalaDefault("-124", ScalaPrimitive.Long) should be("-124L")
    }

    describe ("default: 2014-03-14, datatype: date-iso8601") {
      it("behaves for Joda-Time") {
        ScalaUtil.scalaDefault("2014-03-14", ScalaPrimitive.DateIso8601Joda) should be("""_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate("2014-03-14")""")
      }
      it("behaves for java.time") {
        ScalaUtil.scalaDefault("2014-03-14", ScalaPrimitive.DateIso8601Java) should be("""_root_.java.time.LocalDate.parse("2014-03-14")""")
      }
    }

    describe ("default: 2014-03-14T12:13:15Z, datatype: date-time-iso8601") {
      it("behaves for Joda-Time") {
        ScalaUtil.scalaDefault("2014-03-14T12:13:15Z", ScalaPrimitive.DateTimeIso8601Joda) should be {
          """_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime("2014-03-14T12:13:15Z")"""
        }
      }
      it("behaves for java.time") {
          ScalaUtil.scalaDefault("2014-03-14T12:13:15Z", ScalaPrimitive.DateTimeIso8601JavaInstant) should be {
            """_root_.java.time.OffsetDateTime.parse("2014-03-14T12:13:15Z").toInstant"""
          }
      }
    }

    it ("default: 123453.8, datatype: decimal") {
      ScalaUtil.scalaDefault("123453.8", ScalaPrimitive.Decimal) should be("123453.8")
    }

    it ("default: foo, datatype: string") {
      ScalaUtil.scalaDefault("foo", ScalaPrimitive.String) should be(""""foo"""")
    }

    it ("default: 956169ba-d25e-11e4-8cf3-2bf83a2580ee, datatype: uuid") {
      ScalaUtil.scalaDefault("956169ba-d25e-11e4-8cf3-2bf83a2580ee", ScalaPrimitive.Uuid) should be(
        """_root_.java.util.UUID.fromString("956169ba-d25e-11e4-8cf3-2bf83a2580ee")"""
      )
    }

    it ("""default: ["foo"], datatype: [string]""") {
      ScalaUtil.scalaDefault("""["foo"]""", ScalaDatatype.List(ScalaPrimitive.String)) should be("""scala.List("foo")""")
    }

    it ("default: [], datatype: [string]") {
      ScalaUtil.scalaDefault("""[]""", ScalaDatatype.List(ScalaPrimitive.String)) should be("""Nil""")
    }

    it ("""default: {"name": "foo"}, datatype: map[integer]""") {
      ScalaUtil.scalaDefault("""{"name": "foo"}""", ScalaDatatype.Map(ScalaPrimitive.String)) should be("""Map("name" -> "foo")""")
    }

    it ("empty map, datatype: map[integer]") {
      ScalaUtil.scalaDefault("""{}""", ScalaDatatype.Map(ScalaPrimitive.String)) should be("""Map.empty""")
    }
  }

}
