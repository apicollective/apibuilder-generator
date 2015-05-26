package scala.models

import scala.generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class Play2JsonSpec extends FunSpec with ShouldMatchers {

  describe("for models with lists") {

    val json = TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "unions": [],
      "enums": [],
      "resources": [],
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
      val ssd = ScalaService(TestHelper.service(json))
      val model = ssd.models.head
      TestHelper.assertEqualsFile(
        "/play2-json-spec-model-readers.txt",
        Play2Json(ssd).fieldReaders(model)
      )
    }

  }

  describe("quality schema") {

    lazy val quality = new ScalaService(TestHelper.parseFile("/examples/quality.json"))

    describe("plan") {

      lazy val plan = quality.models.find(_.name == "Plan").get

      it("readers") {
        TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-plan-readers.txt",
          Play2Json(quality).readers(plan)
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-plan-writers.txt",
          Play2Json(quality).writers(plan)
        )
      }
    }

    describe("healthcheck") {

      lazy val healthcheck = quality.models.find(_.name == "Healthcheck").get

      it("readers") {
        TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-healthcheck-readers.txt",
          Play2Json(quality).readers(healthcheck)
        )
      }

      it("writers") {
        TestHelper.assertEqualsFile(
          "/generators/play-2-json-spec-quality-healthcheck-writers.txt",
          Play2Json(quality).writers(healthcheck)
        )
      }
    }
  }

}
