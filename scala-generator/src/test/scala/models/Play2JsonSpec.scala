package scala.models

import scala.generator.ScalaService
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Play2JsonSpec extends AnyFunSpec with Matchers {

  private def play2Json(ssd: ScalaService): Play2Json = Play2Json(ssd, scala3Support = false)

  describe("for models with lists") {

    val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "enums": [],
      "resources": [],
      "attributes": [],
      "models": [
        {
          "name": "content",
          "plural": "contents",
          "attributes": [],
          "fields": [
            { "name": "required_tags", "type": "[string]", "required": true, "attributes": [] },
            { "name": "optional_tags", "type": "[string]", "required": false, "attributes": [] },
            { "name": "data", "type": "map[long]", "required": false, "attributes": [] }
          ]
        }
      ]
    """)

    it("generates valid json readers") {
      val ssd = ScalaService(models.TestHelper.service(json))
      val model = ssd.models.head
      models.TestHelper.assertEqualsFile(
        "/play2-json-spec-model-readers.txt",
        play2Json(ssd).fieldReaders(model)
      )
    }

  }

  describe("For required, defaulted fields") {
    val json1 = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "enums": [],
      "resources": [],
      "attributes": [],
      "models": [
        {
          "name": "a",
          "plural": "as",
          "attributes": [],
          "fields": [
            { "name": "one", "type": "string", "required": true, "default":"adefault", "attributes": [] }
          ]
        }
      ]
    """)

    val json2 = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "enums": [],
      "resources": [],
      "attributes": [],
      "models": [
        {
          "name": "b",
          "plural": "bs",
          "attributes": [],
          "fields": [
            { "name": "one", "type": "[string]", "required": true, "default":"[]", "attributes": [] },
            { "name": "two", "type": "integer", "required": true, "default":"7", "attributes": [] },
            { "name": "three", "type": "date-iso8601", "required": true, "default":"2008-09-15T15:53:00", "attributes": [] },
            { "name": "four", "type": "boolean", "required": true, "default":"true", "attributes": [] }
          ]
        }
      ]
    """)

    it("generates valid json readers for basic objects") {
      val ssd = ScalaService(models.TestHelper.service(json1))
      val model = ssd.models.head
      play2Json(ssd).fieldReaders(model) should be (
        """(__ \ "one").read[String].map { x => new A(one = x) }""".stripMargin)
    }

    it("generates valid json readers for complex objects") {
      val ssd = ScalaService(models.TestHelper.service(json2))
      val model = ssd.models.head
      play2Json(ssd).fieldReaders(model) should be (
        """for {
          |  one <- (__ \ "one").read[Seq[String]]
          |  two <- (__ \ "two").read[Int]
          |  three <- (__ \ "three").read[_root_.org.joda.time.LocalDate]
          |  four <- (__ \ "four").read[Boolean]
          |} yield B(one, two, three, four)""".stripMargin)
    }
  }

  describe("For optional, defaulted fields") {
    val json1 = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "enums": [],
      "resources": [],
      "attributes": [],
      "models": [
        {
          "name": "a",
          "plural": "as",
          "attributes": [],
          "fields": [
            { "name": "one", "type": "string", "required": false, "default":"adefault", "attributes": [] }
          ]
        }
      ]
    """)

    val json2 = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "enums": [],
      "resources": [],
      "attributes": [],
      "models": [
        {
          "name": "b",
          "plural": "bs",
          "attributes": [],
          "fields": [
            { "name": "one", "type": "[string]", "required": false, "default":"[]", "attributes": [] },
            { "name": "two", "type": "integer", "required": false, "default":"7", "attributes": [] },
            { "name": "three", "type": "date-iso8601", "required": false, "default":"2008-09-15T15:53:00", "attributes": [] },
            { "name": "four", "type": "boolean", "required": false, "default":"true", "attributes": [] }
          ]
        }
      ]
    """)

    it("generates valid json readers for basic objects") {
      val ssd = ScalaService(models.TestHelper.service(json1))
      val model = ssd.models.head
      play2Json(ssd).fieldReaders(model) should be (
        """(__ \ "one").readWithDefault[String]("adefault").map { x => new A(one = x) }""".stripMargin)
    }

    it("generates valid json readers for complex objects") {
      val ssd = ScalaService(models.TestHelper.service(json2))
      val model = ssd.models.head
      play2Json(ssd).fieldReaders(model) should be (
        """for {
          |  one <- (__ \ "one").readWithDefault[Seq[String]](Nil)
          |  two <- (__ \ "two").readWithDefault[Int](7)
          |  three <- (__ \ "three").readWithDefault[_root_.org.joda.time.LocalDate](_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate("2008-09-15T15:53:00"))
          |  four <- (__ \ "four").readWithDefault[Boolean](true)
          |} yield B(one, two, three, four)""".stripMargin)
    }
  }

  describe("quality schema") {

    lazy val quality = new ScalaService(models.TestHelper.parseFile("/examples/quality.json"))
    lazy val play2Json = Play2Json(quality, scala3Support = false)

    describe("plan") {

      lazy val plan = quality.models.find(_.name == "Plan").get

      it("readers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-plan-readers.txt",
          play2Json.readers(plan)
        )
      }

      it("writers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-plan-writers.txt",
          play2Json.writers(plan)
        )
      }
    }

    describe("healthcheck") {

      lazy val healthcheck = quality.models.find(_.name == "Healthcheck").get

      it("readers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-healthcheck-readers.txt",
          play2Json.readers(healthcheck)
        )
      }

      it("writers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-healthcheck-writers.txt",
          play2Json.writers(healthcheck)
        )
      }
    }
  }

}
