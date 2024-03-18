package generator.elm

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Service
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ElmGeneratorSpec extends AnyFunSpec with Matchers
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

  private[this] def setupValid(service: Service): Seq[File] = {
    rightOrErrors {
      ElmGenerator.invoke(
        makeInvocationForm(service = service)
      )
    }
  }

  private[this] def genModels(service: Service): String = {
    expectValid {
      ElmGenerator().generateModels(GenArgs(service))
    }
  }

  it("invoke must returns errors") {
    leftOrErrors {
      ElmGenerator.invoke(
        makeInvocationForm()
      )
    }
  }

  it("generates nice filename") {
    setupValid(
      makeService(
        name = "foo",
        namespace = "io.apibuilder",
        models = Seq(makeModel("bar"))
      )
    ).head.name mustBe "Generated/IoApibuilder.elm"
  }

  it("enum") {
    val file = setupValid(
      makeService(
        name = "foo",
        namespace = "io.apibuilder",
        models = Seq(makeModel()),
        enums = Seq(makeEnum(
          name = "newsletter_key",
          plural = "newsletter_keys",
          values = Seq(
            makeEnumValue(name = "general"),
            makeEnumValue(name = "billing"),
          )
        ))
      )
    )
    println(s"File: $file")
  }
}
