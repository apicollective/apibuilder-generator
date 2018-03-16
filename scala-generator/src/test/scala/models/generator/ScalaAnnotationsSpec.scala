package models.generator

import org.scalatest.{FunSpec, Matchers}

import scala.generator.{ScalaEnums, ScalaService}
import scala.models.Play2Json

class ScalaAnnotationsSpec extends FunSpec with Matchers {

  describe("for a model with 2 enum fields") {

    val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "resources": [],
      "attributes": [],

      "enums": [
        {
          "name": "age_group",
          "plural": "age_groups",
          "attributes": [],
          "values": [
            { "name": "twenties", "attributes": [] },
            { "name": "thirties", "attributes": [] }
          ]
        },
        {
          "name": "genre",
          "plural": "genres",
          "attributes": [],
          "values": [
            { "name": "Classical", "attributes": [] },
            { "name": "Jazz", "attributes": [] }
          ]
        }
      ],

      "models": [
        {
          "name": "user",
          "plural": "users",
          "attributes": [],
          "fields": [
            { "name": "age_group", "type": "age_group", "required": true, "attributes": [] },
            { "name": "music", "type": "genre", "required": true, "attributes": [] }
          ]
        }
      ]
    """)

    lazy val ssd = ScalaService(models.TestHelper.service(json))

    it("generates valid models") {
      val enums = ssd.enums.map { ScalaEnums(ssd, _).build() }.mkString("\n\n")
      models.TestHelper.assertEqualsFile("/play2enums-example.txt", enums)
    }

    it("generates valid json conversions") {
      val jsonConversions = Play2Json(ssd).generateEnums()
      models.TestHelper.assertEqualsFile("/play2enums-json-example.txt", jsonConversions)
    }
  }

}
