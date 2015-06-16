package ruby.models

import java.io.File
import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models.{Enum, EnumValue}

import org.scalatest.{ FunSpec, Matchers }

class RubyClientGeneratorSpec extends FunSpec with Matchers {

  it("enumName") {
    RubyClientGenerator.enumName("CANCEL_REQUEST") should be("cancel_request")
    RubyClientGenerator.enumName("cancel_request") should be("cancel_request")
    RubyClientGenerator.enumName("cancelRequest") should be("cancel_request")
    RubyClientGenerator.enumName("CancelRequest") should be("cancel_request")
  }

  it("classDeclaration") {
    RubyClientGenerator.classDeclaration("RegisteredUser", None) should be("class RegisteredUser")
    RubyClientGenerator.classDeclaration("RegisteredUser", Some("User")) should be("class RegisteredUser < User")
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

      models.TestHelper.assertEqualsFile("/ruby-gem-enums.txt", RubyClientGenerator.generateEnum(enum, None))
    }

  }

  it("generate ruby") {
    val service = models.TestHelper.referenceApiService
    RubyClientGenerator.invoke(InvocationForm(service = service, userAgent = Some("gilt 0.0.1-test"))) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        models.TestHelper.assertEqualsFile("/ruby-client-generator-gilt-0.0.1-test.txt", code)
      }
    }
  }
}
