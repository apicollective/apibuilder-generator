package generator

import org.scalatest.{ ShouldMatchers, FunSpec }

class ScalaUtilSpec extends FunSpec with ShouldMatchers {

  it("extendsClause") {
    ScalaUtil.extendsClause(Nil) should be(None)
    ScalaUtil.extendsClause(Seq("Foo")).get should be("extends Foo")
    ScalaUtil.extendsClause(Seq("Foo", "Bar")).get should be("extends Bar with Foo")
    ScalaUtil.extendsClause(Seq("Foo", "Bar", "Baz")).get should be("extends Bar with Baz with Foo")
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

  }

  describe("scalaDefault") {
    it ("should fail for datatype: object") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Object)
      }
      ex.getMessage should be("parsing default `` for datatype Object")
    }

    it ("should fail for datatype: unit") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Unit)
      }
      ex.getMessage should be("parsing default `` for datatype Unit")
    }

    it ("default: foo, datatype: enum") {
      ScalaUtil.scalaDefault("foo", ScalaPrimitive.Enum("test", "test")) should be("""test.test("foo")""")
    }

    it ("should fail for datatype: model") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Model("test", "test"))
      }
      ex.getMessage should be("parsing default `` for datatype Model(test,test)")
    }

    it ("should fail for datatype: union") {
      val ex = intercept[Exception] {
        ScalaUtil.scalaDefault("", ScalaPrimitive.Union("test", "test"))
      }
      ex.getMessage should be("parsing default `` for datatype Union(test,test)")
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

    it ("default: 124, datatype: long") {
      ScalaUtil.scalaDefault("124", ScalaPrimitive.Long) should be("124")
    }

    it ("default: -124, datatype: long") {
      ScalaUtil.scalaDefault("-124", ScalaPrimitive.Long) should be("-124")
    }

    it ("default: 2014-03-14, datatype: date-iso8601") {
      ScalaUtil.scalaDefault("2014-03-14", ScalaPrimitive.DateIso8601) should be("new _root_.org.joda.time.LocalDate(2014, 3, 14)")
    }

    it ("default: 2014-03-14T12:13:15Z, datatype: date-time-iso8601") {
      ScalaUtil.scalaDefault("2014-03-14T12:13:15Z", ScalaPrimitive.DateTimeIso8601) should be {
        """_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime("2014-03-14T12:13:15Z")"""
      }
    }

    it ("default: 123453.8, datatype: decimal") {
      ScalaUtil.scalaDefault("123453.8", ScalaPrimitive.Decimal) should be("123453.8")
    }

    it ("default: foo, datatype: string") {
      ScalaUtil.scalaDefault("foo", ScalaPrimitive.String) should be(""""foo"""")
    }

    it ("default: 956169ba-d25e-11e4-8cf3-2bf83a2580ee, datatype: uuid") {
      ScalaUtil.scalaDefault("956169ba-d25e-11e4-8cf3-2bf83a2580ee", ScalaPrimitive.Uuid) should be("new UUID(-7682743238203141660, -8290234143931531026)")
    }

    it ("""default: ["foo"], datatype: [string]""") {
      ScalaUtil.scalaDefault("""["foo"]""", ScalaDatatype.List(ScalaPrimitive.String)) should be("""Seq("foo")""")
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
