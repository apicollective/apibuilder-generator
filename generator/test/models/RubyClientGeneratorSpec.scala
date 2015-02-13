package models

import java.io.File
import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.{Enum, EnumValue}

import org.scalatest.{ FunSpec, Matchers }

class RubyClientGeneratorSpec extends FunSpec with Matchers {

  it("enumName") {
    RubyClientGenerator.enumName("CANCEL_REQUEST") should be("cancel_request")
    RubyClientGenerator.enumName("cancel_request") should be("cancel_request")
    RubyClientGenerator.enumName("cancelRequest") should be("cancel_request")
    RubyClientGenerator.enumName("CancelRequest") should be("cancel_request")
  }

  describe("generateEnumClass") {

    it("for enum with multiple values") {
      val enum = Enum(
        name = "age_group",
        plural = "age_groups",
        description = None,
        values = Seq(
          EnumValue(
            name = "Thirties",
            description = None
          ),
          EnumValue(
            name = "Forties",
            description = None
          )
        )
      )
            
      TestHelper.assertEqualsFile("test/resources/ruby-gem-enums.txt", RubyClientGenerator.generateEnum(enum))
    }

  }

  it("generate ruby") {
    val service = TestHelper.referenceApiService
    val code = RubyClientGenerator.invoke(InvocationForm(service = service, userAgent = Some("gilt 0.0.1-test")))
    TestHelper.assertEqualsFile("test/resources/ruby-client-generator-gilt-0.0.1-test.txt", code)
  }
}
