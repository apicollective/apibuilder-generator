package generator

import models.{Play2Json, TestHelper}
import com.gilt.apidoc.generator.v0.models.InvocationForm
import org.scalatest.{ ShouldMatchers, FunSpec }

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
    TestHelper.assertEqualsFile("test/resources/scala-union-example.txt", code)
  }

  it("raises error if you try to generate json for a model that is part of union type") {
    val registeredUser = ssd.models.find(_.name == "RegisteredUser").get
    intercept[AssertionError] {
      Play2Json("test").generate(registeredUser)
    }.getMessage should be("assertion failed: Cannot generate play json for models that are part of union types. User must use the json serialization for the parent union type")
  }

}
