package generator.elm

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Field, Service}
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
    ElmGenerator().generateModels(service)
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
    ).head.name mustBe "IoApibuilderFoo.elm"
  }

  it("filename dedups service name") {
    setupValid(
      makeService(
        name = "foo",
        namespace = "io.apibuilder.foo",
        models = Seq(makeModel("bar"))
      )
    ).head.name mustBe "IoApibuilderFoo.elm"
  }

  describe("generate models") {
    def setup(fields: Seq[Field]) = {
      genModels(
        makeService(
          models = Seq(makeModel(
            name = "user",
            fields = fields
          ))
        )
      )
    }

    it("with 1 field") {
      setup(
        Seq(makeField("id", `type` = "string"))
      ) mustBe
        """
          |public record User (
          |  string Id
          |);
          |""".stripMargin.trim
    }

    it("with 2 fields") {
      setup(
        Seq(
          makeField("id", `type` = "string"),
          makeField("name", `type` = "string"),
          makeField("age", `type` = "long", required = false)
        )
      ) mustBe
        """
          |public record User (
          |  string Id,
          |  string Name,
          |  long? Age
          |);
          |""".stripMargin.trim
    }
  }

}
