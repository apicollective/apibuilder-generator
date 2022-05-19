package generator.csharp

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.Service
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class CSharpGeneratorSpec extends AnyFunSpec with Matchers
  with ServiceHelpers
  with TestHelpers
{

  private[this] def makeInvocationForm(service: Service = makeService()): InvocationForm = {
    InvocationForm(
      service = service,
      attributes = Nil,
      userAgent = None,
      importedServices = None
    )
  }

  it("invoke must returns errors") {
    leftOrErrors {
      CSharpGenerator.invoke(
        makeInvocationForm()
      )
    }
  }

  it("generate mdoels") {
    rightOrErrors {
      CSharpGenerator.invoke(
        makeInvocationForm(
          service = makeService(
            models = Seq(makeModel(
              name = "user",
              fields = Seq(makeField("id", `type` = "string"))
            ))
          )
        )
      )
    }.head.contents mustBe """
      |public record User (
      |  string Id
      |);
      |""".stripMargin.trim
  }

}
