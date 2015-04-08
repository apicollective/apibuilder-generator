package models

import generator.{ScalaCaseClasses, ScalaClientMethodConfigs, ScalaService}
import com.gilt.apidoc.generator.v0.models.InvocationForm
import org.scalatest.{ShouldMatchers, FunSpec}

class ScalaUnionSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play23("test.apidoc")

  describe("models") {

    val json = TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "enums": [],
      "resources": [],

      "unions": [
        {
          "name": "user",
          "plural": "users",
          "types": [
            { "type": "registered_user" },
            { "type": "guest_user" }
          ]
        }
      ],

      "models": [
        {
          "name": "registered_user",
          "plural": "registered_users",
          "fields": [
            { "name": "id", "type": "long", "required": true },
            { "name": "email", "type": "string", "required": true },
            { "name": "name", "type": "string", "required": false },
            { "name": "foo", "type": "string", "required": true }
          ]
        },
        {
          "name": "guest_user",
          "plural": "guest_users",
          "fields": [
            { "name": "id", "type": "long", "required": true },
            { "name": "email", "type": "string", "required": true },
            { "name": "name", "type": "string", "required": false },
            { "name": "bar", "type": "string", "required": true }
          ]
        }
      ]
  """)

    lazy val service = TestHelper.service(json)
    lazy val ssd = ScalaService(service)

    it("generates valid models") {
      ScalaCaseClasses.invoke(InvocationForm(service), addHeader = false) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(code) => {
          TestHelper.assertEqualsFile("test/resources/scala-union-models-case-classes.txt", code)
        }
      }
    }

    it("generates valid readers for the union type itself") {
      val user = ssd.unions.find(_.name == "User").get
      val code = Play2Json(ssd).readers(user)
      TestHelper.assertEqualsFile("test/resources/scala-union-models-json-union-type-readers.txt", code)
    }

    it("generates valid writers for the union type itself") {
      val user = ssd.unions.find(_.name == "User").get
      val code = Play2Json(ssd).writers(user)
      TestHelper.assertEqualsFile("test/resources/scala-union-models-json-union-type-writers.txt", code)
    }

    it("codegen") {
      val code = Play2Json(ssd).generate()
      TestHelper.assertEqualsFile("test/resources/scala-union-models-json.txt", code)
    }
  }

  describe("enums") {

    val json = TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "models": [],
      "resources": [],

      "enums": [
        {
          "name": "member_type",
          "plural": "member_types",
          "values": [
            { "name": "Registered" },
            { "name": "Guest" }
          ]
        },
        {
          "name": "role_type",
          "plural": "role_types",
          "values": [
            { "name": "Admin" }
          ]
        }
      ],

      "unions": [
        {
          "name": "user_type",
          "plural": "user_types",
          "types": [
            { "type": "member_type" },
            { "type": "role_type" }
          ]
        }
      ]
    """)

    lazy val service = TestHelper.service(json)
    lazy val ssd = ScalaService(service)

    it("codegen") {
      val code = Play2Json(ssd).generate()
      TestHelper.assertEqualsFile("test/resources/scala-union-enums-json.txt", code)
    }
  }

}
