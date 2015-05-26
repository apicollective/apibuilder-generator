package generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import ruby.models.RubyClientGenerator

import ruby.models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class IllegalNonRequiredWithDefaultSpec extends FunSpec with Matchers {

  // TODO make sure this is right for ruby; was copied from scala
  it("fails, because a non-required field has a default") {
    val ex = intercept[Exception] {
      RubyClientGenerator.invoke(InvocationForm(service = TestHelper.illegalNonRequiredWithDefaultService))
    }
    ex.getMessage should be(s"""parsing default `["foo"]` for datatype Option(List(String))""")
  }

}


