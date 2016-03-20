package scala.generator

import com.bryzek.apidoc.generator.v0.models.InvocationForm

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class IllegalNonRequiredWithDefaultSpec extends FunSpec with Matchers {

  val json = buildJson("""

      "models": [
        {
          "name": "user",
          "plural": "users",
          "fields": [
            { "name": "age_group", "type": "age_group", "required": false, "default": "21-25" }
          ]
        }
      ]

  """)

  it("fails, because a non-required field has a default") {
    val service = TestHelper.service(json)
    val ex = intercept[Exception] {
      new ScalaService(service)
    }
    ex.getMessage should be(s"""parsing default `["foo"]` for datatype Option(List(String))""")
  }

}


