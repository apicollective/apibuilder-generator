package models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.models.Play28ClientGenerator

class UndefinedUnionWithInterfaceSpec extends AnyFunSpec with Matchers {

  private[this] val json: String = models.TestHelper.buildJson("""
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
          "interfaces": ["user"],
          "types": [
            { "type": "registered_user", "attributes": [] }
          ]
        }
      ],

      "interfaces": [
        {
          "name": "user",
          "plural": "user",
          "attributes": [],
          "fields": [
            { "name": "description", "type": "string", "required": false, "attributes": [] }
          ]
        }
      ],

      "models": [
        {
          "name": "registered_user",
          "plural": "registered_users",
          "attributes": [],
          "fields": [
            { "name": "description", "type": "long", "required": true, "attributes": [] }
          ]
        }
      ]
  """)


  it("codegen") {
    val form = InvocationForm(
      models.TestHelper.service(json),
      attributes = Nil,
      None
    )
    val Right(files) = Play28ClientGenerator.invoke(form)
    models.TestHelper.assertEqualsFile("/undefined-union-with-interface-spec.txt", files.head.contents)
  }

}
