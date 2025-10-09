package models

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.models.Play29Scala3ClientGenerator

class UnionWithImportedInterfaceSpec extends AnyFunSpec with Matchers with ServiceHelpers with TestHelpers {

  private lazy val eventService = makeService(
    name = "event_service",
    namespace = "test",
    version = "1",
    interfaces = Seq(makeInterface(
      name = "event",
      plural = "events",
      fields = Seq(makeField(name = "timestamp"))
    ))
  )

  private lazy val service = makeService(
    name = "sse",
    namespace = "ns",
    version = "1",
    imports = Seq(makeImport(eventService)),
    unions = Seq(makeUnion(
      name = "example",
      plural = "examples",
      interfaces = Seq("test.interfaces.event")
    ))
  )

  it("generates expected code") {
    val sourceFiles = rightOrErrors {
      Play29Scala3ClientGenerator.invoke(InvocationForm(service = service, importedServices = Some(Seq(eventService))))
    }
    sourceFiles.size shouldBe 1
    models.TestHelper.assertEqualsFile("/union-with-imported-interface.txt", sourceFiles.head.contents)
  }

}
