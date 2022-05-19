package generator.csharp

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class CSharpGeneratorSpec extends AnyFunSpec with Matchers
  with ServiceHelpers
  with TestHelpers
{

  private[this] def makeInvocationForm(): InvocationForm = {
    InvocationForm(
      service = makeService(),
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

}
