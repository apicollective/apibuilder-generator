package generator

import com.gilt.apidoc.spec.v0.models.Service
import models.TestHelper
import org.scalatest.{ ShouldMatchers, FunSpec }

class ScalaEnumsSpec extends FunSpec with ShouldMatchers {

  describe("for a model with 2 enum fields") {

    val json = TestHelper.buildJson("""
      "enums": [
        {
          "name": "age_group",
          "plural": "age_groups",
          "values": [
            { "name": "twenties" },
            { "name": "thirties" }
          ]
        },
        {
          "name": "genre",
          "plural": "genres",
          "values": [
            { "name": "Classical" },
            { "name": "Jazz" }
          ]
        }
      ],

      "models": [
        {
          "name": "user",
          "plural": "users",
          "fields": [
            { "name": "age_group", "type": "age_group", "required": true },
            { "name": "music", "type": "genre", "required": true }
          ]
        }
      ]
    """)

    lazy val ssd = TestHelper.scalaService(json)

    it("generates valid models") {
      val enums = ssd.enums.map { ScalaEnums(ssd, _).build() }.mkString("\n\n")
      TestHelper.assertEqualsFile("test/resources/play2enums-example.txt", enums)
    }

    it("generates valid json conversions") {
      val jsonConversions = ssd.enums.map { ScalaEnums(ssd, _).buildJson() }.mkString("\n\n")
      TestHelper.assertEqualsFile("test/resources/play2enums-json-example.txt", jsonConversions)
    }
  }


}
