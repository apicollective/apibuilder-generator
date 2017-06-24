package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import ning.Ning18ClientGenerator
import org.scalatest.{ShouldMatchers, FunSpec}

class ExampleUnionTypesWithDiscriminatorSpec extends FunSpec with ShouldMatchers {

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
