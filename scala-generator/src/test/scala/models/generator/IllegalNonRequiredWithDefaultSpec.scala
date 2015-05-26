package scala.generator

import com.gilt.apidoc.generator.v0.models.InvocationForm

import scala.models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class IllegalNonRequiredWithDefaultSpec extends FunSpec with Matchers {

  it("fails, because a non-required field has a default") {
    val ex = intercept[Exception] {
      new ScalaService(TestHelper.illegalNonRequiredWithDefaultService)
    }
    ex.getMessage should be(s"""parsing default `["foo"]` for datatype Option(List(String))""")
  }

}


