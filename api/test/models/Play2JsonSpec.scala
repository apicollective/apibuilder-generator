package models

import core._
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
      val ssd = new ScalaService(ServiceBuilder(json))
      val model = ssd.models.head
      TestHelper.assertEqualsFile(
        "test/resources/play2-json-spec-model-readers.txt",
        Play2Json("Api Doc Test", "content", model).fieldReaders(model)
      )
    }

  }

  describe("quality schema") {

    lazy val quality = new ScalaService(TestHelper.parseFile("test/resources/examples/quality.json").serviceDescription.get)

    describe("plan") {

      lazy val plan = quality.models.find(_.name == "Plan").get

      it("readers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-plan-readers.txt",
          Play2Json(quality.name, "plan", plan).readers()
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-plan-writers.txt",
          Play2Json(quality.name, "plan", plan).writers()
        )
      }
    }

    describe("healthcheck") {

      lazy val healthcheck = quality.models.find(_.name == "Healthcheck").get

      it("readers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-healthcheck-readers.txt",
          Play2Json(quality.name, "healthcheck", healthcheck).readers()
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "test/resources/generators/play-2-json-spec-quality-healthcheck-writers.txt",
          Play2Json(quality.name, "healthcheck", healthcheck).writers()
        )
      }
    }
  }

}
