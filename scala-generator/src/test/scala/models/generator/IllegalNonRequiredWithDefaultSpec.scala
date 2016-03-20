package scala.generator

import com.bryzek.apidoc.generator.v0.models.InvocationForm

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class IllegalNonRequiredWithDefaultSpec extends FunSpec with Matchers {

  val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "unions": [],
      "resources": [],
      "attributes": [],

      "models": [
        {
          "name": "user",
          "plural": "users",
          "attributes": [],
          "fields": [
            { "name": "age", "type": "string", "required": false, "default": "21", "attributes": [] }
          ]
        }
      ]
  """)

  it("fails, because a non-required field has a default") {
    val service = TestHelper.service(json)
    val ex = intercept[Exception] {
      new ScalaService(service)
    }
    ex.getMessage should be(s"""parsing default `21` for datatype Option(String)""")
  }

}


