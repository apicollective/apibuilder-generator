package models

import helpers.ServiceHelpers
import io.apibuilder.spec.v0.models.Field
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.ScalaService

class UndefinedUnionWithInterfaceSpec extends AnyFunSpec with Matchers with ServiceHelpers {

  private[this] def build(fields: Seq[Field]): ScalaService = {
    ScalaService(
      makeService(
        interfaces = Seq(
          makeInterface(name = "user", fields = fields)
        ),
        unions = Seq(
          makeUnion(
            name = "user",
            interfaces = Seq("user"),
            types = Seq(makeUnionType("registered_user")),
          )
        ),
        models = Seq(
          makeModel(
            name = "registered_user",
          )
        )
      )
    )
  }

  private[this] def fields(service: ScalaService): Seq[String] = {
    service.unions.map(_.undefinedType).head.model.model.fields.map(_.name)
  }

  it("defaults to 'description'") {
    fields(build(Nil)) shouldBe Seq("description")
  }

  it("aliases description field") {
    fields(build(Seq(
      makeField("description", required = false),
    ))) shouldBe Seq("typeDescription")
  }

  it("aliases description and typeDescription field") {
    fields(build(Seq(
      makeField("description", required = false),
      makeField("type_description", required = false),
    ))) shouldBe Seq("type2Description")
  }

}
