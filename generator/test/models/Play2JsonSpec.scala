package models

import generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class Play2JsonSpec extends FunSpec with ShouldMatchers {

  describe("for models with lists") {

    val json = TestHelper.buildJson("""
      "models": [
        {
          "name": "content",
          "plural": "contents",
          "fields": [
            { "name": "required_tags", "type": "[string]", "required": true },
            { "name": "optional_tags", "type": "[string]", "required": false },
            { "name": "data", "type": "map[long]", "required": false }
          ]
        }
      ]
    """)

    it("generates valid json readers") {
      val ssd = TestHelper.scalaService(json)
      val model = ssd.models.head
      TestHelper.assertEqualsFile(
        "test/resources/play2-json-spec-model-readers.txt",
        Play2Json("Api Doc Test", model).fieldReaders()
      )
    }

  }

  describe("quality schema") {

    lazy val quality = new ScalaService(TestHelper.parseFile("test/resources/examples/quality.json"))

    describe("plan") {

      lazy val plan = quality.models.find(_.name == "Plan").get

      it("readers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-plan-readers.txt",
          Play2Json(quality.name, plan).readers()
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-plan-writers.txt",
          Play2Json(quality.name, plan).writers()
        )
      }
    }

    describe("healthcheck") {

      lazy val healthcheck = quality.models.find(_.name == "Healthcheck").get

      it("readers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-healthcheck-readers.txt",
          Play2Json(quality.name, healthcheck).readers()
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-healthcheck-writers.txt",
          Play2Json(quality.name, healthcheck).writers()
        )
      }
    }
  }

}
