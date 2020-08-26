package models.generator

import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.Interface
import models.TestHelper
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

  it("interfacesWithDeprecation") {
    val service = models.TestHelper.parseFile(s"/examples/interfaces.json")
    ScalaCaseClasses.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        TestHelper.assertValidScalaSourceFiles(sourceFiles)
        models.TestHelper.assertEqualsFile(
          "/generators/scala-models-interfaces.txt",
          sourceFiles.head.contents
        )
      }
    }
  }

}
