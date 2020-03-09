package ruby.models

import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.{Enum, EnumValue}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RubyClientGeneratorSpec extends AnyFunSpec with Matchers {

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

    it("for enum with spaces") {
      RubyClientGenerator.enumName("sq m") should be("sq_m")
    }

  }

  it("generate ruby") {
    val service = models.TestHelper.referenceApiService
    RubyClientGenerator.invoke(InvocationForm(service = service, userAgent = Some("gilt 0.0.1-test"))) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/ruby-client-generator-gilt-0.0.1-test.txt", sourceFiles.head.contents)
      }
    }
  }
}
