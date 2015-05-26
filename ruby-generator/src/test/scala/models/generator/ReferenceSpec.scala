package generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import ruby.models.RubyClientGenerator

import ruby.models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class ReferenceSpec extends FunSpec with Matchers {

  it("generates expected code for ruby client") {
    RubyClientGenerator.invoke(InvocationForm(service = TestHelper.referenceApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        TestHelper.assertEqualsFile("/generators/reference-spec-ruby-client.txt", code)
      }
    }
  }

}


