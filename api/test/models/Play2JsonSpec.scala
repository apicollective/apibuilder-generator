package models

import generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class Play2JsonSpec extends FunSpec with ShouldMatchers {

  describe("for models with lists") {

    val json = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc Test",

      "models": {
        "content": {
          "fields": [
            { "name": "required_tags", "type": "[string]" },
            { "name": "optional_tags", "type": "[string]", "required": false },
            { "name": "data", "type": "map[long]", "required": false }
          ]
        }
      }
    }
    """

    it("generates valid json readers") {
      val ssd = TestHelper.scalaService(json)
      val model = ssd.models.values.head
      TestHelper.assertEqualsFile(
        "test/resources/play2-json-spec-model-readers.txt",
        Play2Json("Api Doc Test", "Content", model).fieldReaders()
      )
    }

  }

  describe("quality schema") {

    lazy val quality = new ScalaService(TestHelper.parseFile("test/resources/examples/quality.json"))

    describe("plan") {

      lazy val plan = quality.models("Plan")

      it("readers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-plan-readers.txt",
          Play2Json(quality.name, "Plan", plan).readers()
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-plan-writers.txt",
          Play2Json(quality.name, "Plan", plan).writers()
        )
      }
    }

    describe("healthcheck") {

      lazy val healthcheck = quality.models("Healthcheck")

      it("readers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-healthcheck-readers.txt",
          Play2Json(quality.name, "Healthcheck", healthcheck).readers()
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-healthcheck-writers.txt",
          Play2Json(quality.name, "Healthcheck", healthcheck).writers()
        )
      }
    }
  }

}
