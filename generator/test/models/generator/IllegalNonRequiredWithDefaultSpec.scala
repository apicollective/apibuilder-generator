package generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import models.{Play23ClientGenerator, RubyClientGenerator}
import models.ning.Ning18ClientGenerator

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class IllegalNonRequiredWithDefaultSpec extends FunSpec with Matchers {

  it("fails, because a non-required field has a default") {
    val ex = intercept[Exception] {
      new ScalaService(TestHelper.illegalNonRequiredWithDefaultService)
    }
    ex.getMessage should be(s"""parsing default `["foo"]` for datatype Option(List(String))""")
  }

}


