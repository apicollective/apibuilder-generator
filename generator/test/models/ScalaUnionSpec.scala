package models

import generator.{ScalaCaseClasses, ScalaClientMethodConfigs, ScalaService}
import com.gilt.apidoc.generator.v0.models.InvocationForm
import org.scalatest.{ShouldMatchers, FunSpec}

class ScalaUnionSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play23("test.apidoc")

  val json = TestHelper.buildJson("""
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
    val code = ScalaCaseClasses.invoke(InvocationForm(service), addHeader = false)
    TestHelper.assertEqualsFile("test/resources/scala-union-case-classes.txt", code)
  }

  it("generates valid readers for the union type itself") {
    val user = ssd.unions.find(_.name == "User").get
    val code = Play2Json(ssd).readers(user)
    TestHelper.assertEqualsFile("test/resources/scala-union-json-union-type-readers.txt", code)
  }

  it("generates valid writers for the union type itself") {
    val user = ssd.unions.find(_.name == "User").get
    val code = Play2Json(ssd).writers(user)
    TestHelper.assertEqualsFile("test/resources/scala-union-json-union-type-writers.txt", code)
  }

  it("codegen implicits") {
    val code = Play2Json(ssd).generate()
    TestHelper.assertEqualsFile("test/resources/scala-union-json-implicits.txt", code.implicits)
  }

  it("codegen packages") {
    val code = Play2Json(ssd).generate()
    TestHelper.assertEqualsFile("test/resources/scala-union-json-packages.txt", code.packages)
  }

}
