package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.{ScalaCaseClasses, ScalaService}

class ScalaUnionSpec extends AnyFunSpec with Matchers {

  private def play2Json(ssd: ScalaService): Play2Json = Play2Json(ssd, scala3Support = false)

  describe("models") {

    val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "resources": [],
      "attributes": [],

      "unions": [
        {
          "name": "user",
          "plural": "users",
          "attributes": [],
          "types": [
            { "type": "registered_user", "attributes": [] },
            { "type": "guest_user", "attributes": [] }
          ]
        }
      ],

      "models": [
        {
          "name": "registered_user",
          "plural": "registered_users",
          "attributes": [],
          "fields": [
            { "name": "id", "type": "long", "required": true, "attributes": [] },
            { "name": "email", "type": "string", "required": true, "attributes": [] },
            { "name": "name", "type": "string", "required": false, "attributes": [] },
            { "name": "foo", "type": "string", "required": true, "attributes": [] }
          ]
        },
        {
          "name": "guest_user",
          "plural": "guest_users",
          "attributes": [],
          "fields": [
            { "name": "id", "type": "long", "required": true, "attributes": [] },
            { "name": "email", "type": "string", "required": true, "attributes": [] },
            { "name": "name", "type": "string", "required": false, "attributes": [] },
            { "name": "bar", "type": "string", "required": true, "attributes": [] }
          ]
        }
      ]
  """)

    lazy val service = models.TestHelper.service(json)
    lazy val ssd = ScalaService(service)

    it("generates valid models") {
      ScalaCaseClasses.invoke(InvocationForm(service), addHeader = false) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/scala-union-models-case-classes.txt", sourceFiles.head.contents)
        }
      }
    }

    it("generates valid readers for the union type itself") {
      val user = ssd.unions.find(_.name == "User").get
      val code = play2Json(ssd).readers(user)
      models.TestHelper.assertEqualsFile("/scala-union-models-json-union-type-readers.txt", code)
    }

    it("generates valid writers for the union type itself") {
      val user = ssd.unions.find(_.name == "User").get
      val code = play2Json(ssd).writers(user)
      models.TestHelper.assertEqualsFile("/scala-union-models-json-union-type-writers.txt", code)
    }

    it("codegen") {
      val code = play2Json(ssd).generateModelsAndUnions()
      models.TestHelper.assertEqualsFile("/scala-union-models-json.txt", code)
    }
  }

  describe("enums") {

    val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "models": [],
      "resources": [],
      "attributes": [],

      "enums": [
        {
          "name": "member_type",
          "plural": "member_types",
          "attributes": [],
          "values": [
            { "name": "Registered", "attributes": [] },
            { "name": "Guest", "attributes": [] }
          ]
        },
        {
          "name": "role_type",
          "plural": "role_types",
          "attributes": [],
          "values": [
            { "name": "Admin", "attributes": [] }
          ]
        }
      ],

      "unions": [
        {
          "name": "user_type",
          "plural": "user_types",
          "attributes": [],
          "types": [
            { "type": "member_type", "attributes": [] },
            { "type": "role_type", "attributes": [] }
          ]
        }
      ]
    """)

    lazy val service = models.TestHelper.service(json)
    lazy val ssd = ScalaService(service)

    it("codegen") {
      val code = play2Json(ssd).generateModelsAndUnions()
      models.TestHelper.assertEqualsFile("/scala-union-enums-json.txt", code)
    }
  }

}
