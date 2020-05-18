package models.generator

import io.apibuilder.spec.v0.models.Interface
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.{ScalaCaseClasses, ScalaService}

class ScalaCaseClassesInterfacesSpec extends AnyFunSpec with Matchers with helpers.ServiceHelpers {

  private[this] def build(interfaces: Seq[Interface]): ScalaService = {
    ScalaService(
      makeService(interfaces = interfaces)
    )
  }

  it("interfaceWithNoFields") {
    val ssd = build(
      Seq(makeInterface(
        name = "person",
        fields = Nil,
      ))
    )

    models.TestHelper.assertEqualsFile(
      "/generators/ScalaCaseClassesInterfacesSpec.interfaceWithNoFields.json",
      ScalaCaseClasses.generateTrait(ssd.interfaces.head)
     )
  }

  it("interfaceWithSingleField") {
    val ssd = build(
      Seq(makeInterface(
        name = "person",
        fields = Seq(makeField(name = "first")),
      ))
    )

    models.TestHelper.assertEqualsFile(
      "/generators/ScalaCaseClassesInterfacesSpec.interfaceWithSingleField.json",
      ScalaCaseClasses.generateTrait(ssd.interfaces.head)
    )
  }

}
