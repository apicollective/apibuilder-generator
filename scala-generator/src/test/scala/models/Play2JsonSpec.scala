package scala.models

import scala.generator.ScalaService
import org.scalatest.{ Matchers, FunSpec }

class Play2JsonSpec extends FunSpec with Matchers {

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
        Play2Json(ssd).fieldReaders(model)
      )
    }

  }

  describe("quality schema") {

    lazy val quality = new ScalaService(models.TestHelper.parseFile("/examples/quality.json"))

    describe("plan") {

      lazy val plan = quality.models.find(_.name == "Plan").get

      it("readers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-plan-readers.txt",
          Play2Json(quality).readers(plan)
        )
      }

      it("writers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-plan-writers.txt",
          Play2Json(quality).writers(plan)
        )
      }
    }

    describe("healthcheck") {

      lazy val healthcheck = quality.models.find(_.name == "Healthcheck").get

      it("readers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-healthcheck-readers.txt",
          Play2Json(quality).readers(healthcheck)
        )
      }

      it("writers") {
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-healthcheck-writers.txt",
          Play2Json(quality).writers(healthcheck)
        )
      }
    }
  }

}
