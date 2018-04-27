package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{Matchers, FunSpec}

class ExampleUnionTypesWithDiscriminatorSpec extends FunSpec with Matchers {

  private lazy val service = models.TestHelper.parseFile(s"/examples/apidoc-example-union-types-discriminator.json")

  it("generates expected code for play 2.4 client") {
    Play24ClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/union-types-discriminator-service-play-24.txt", sourceFiles.head.contents)
      }
    }
  }

}
