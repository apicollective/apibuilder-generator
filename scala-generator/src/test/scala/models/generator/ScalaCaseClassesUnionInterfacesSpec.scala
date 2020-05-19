package models.generator

import io.apibuilder.spec.v0.models.{Field, Interface}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.{ScalaCaseClasses, ScalaService}

class ScalaCaseClassesUnionInterfacesSpec extends AnyFunSpec with Matchers with helpers.ServiceHelpers {

  private[this] val IdField: Field = makeField(name = "id")

  private[this] def build(
    interface: Interface,
  ): ScalaService = {
    ScalaService(
      makeService(
        namespace = "test",
        version = "0.0.1",
        unions = Seq(
          makeUnion(name = "person", types = Seq(makeUnionType(`type` = "user")), interfaces = Seq(interface.name))
        ),
        models = Seq(
          makeModel(name = "user", fields = Seq(IdField))
        ),
        interfaces = Seq(interface),
      )
    )
  }

  it("interfaceHasSameName") {
    val ssd = build(
      makeInterface(
        name = "person",
        fields = Nil,
      )
    )

    models.TestHelper.assertEqualsFile(
      "/generators/ScalaCaseClassesUnionInterfacesSpec.interfaceHasSameName.json",
      ScalaCaseClasses.generateCode(ssd, userAgent = None).head.contents
    )
  }

  it("interfaceHasSameNameAndFields") {
    val ssd = build(
      makeInterface(
        name = "person",
        fields = Seq(IdField),
      )
    )

    models.TestHelper.assertEqualsFile(
      "/generators/ScalaCaseClassesUnionInterfacesSpec.interfaceHasSameNameAndFields.json",
      ScalaCaseClasses.generateCode(ssd, userAgent = None).head.contents
    )
  }

  it("interfaceHasNoFields") {
    val ssd = build(
      makeInterface(
        name = "foo",
        fields = Nil,
      )
    )

    models.TestHelper.assertEqualsFile(
      "/generators/ScalaCaseClassesUnionInterfacesSpec.interfaceHasNoFields.json",
      ScalaCaseClasses.generateCode(ssd, userAgent = None).head.contents
    )
  }

  it("interfaceHasFields") {
    val ssd = build(
      makeInterface(
        name = "foo",
        fields = Seq(IdField),
      )
    )

    models.TestHelper.assertEqualsFile(
      "/generators/ScalaCaseClassesUnionInterfacesSpec.interfaceHasFields.json",
      ScalaCaseClasses.generateCode(ssd, userAgent = None).head.contents
    )
  }

}
