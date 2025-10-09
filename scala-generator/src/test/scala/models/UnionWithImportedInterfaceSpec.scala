package models

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.models.Play29Scala3ClientGenerator

class UnionWithImportedInterfaceSpec extends AnyFunSpec with Matchers with ServiceHelpers with TestHelpers {

  private lazy val eventService = makeService(
    interfaces = Seq(makeInterface(
      name = "event",
      fields = Seq(makeField(name = "timestamp"))
    ))
  )

  private lazy val service = makeService(
    unions = Seq(makeUnion(
      name = "example",
      interfaces = Seq(s"${eventService.namespace}.interfaces.event")
    ))
  )

  it("generates expected code") {
    val sourceFiles = rightOrErrors {
      Play29Scala3ClientGenerator.invoke(InvocationForm(service = service, importedServices = Some(Seq(eventService))))
    }
    sourceFiles.size shouldBe 1
    models.TestHelper.assertEqualsFile("/example-union-types-play-29-scala-3.txt", sourceFiles.head.contents)
  }

}
