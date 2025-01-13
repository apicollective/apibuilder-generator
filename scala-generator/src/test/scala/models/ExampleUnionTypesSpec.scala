package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import ning.Ning18ClientGenerator
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ExampleUnionTypesSpec extends AnyFunSpec with Matchers {

  private lazy val service = models.TestHelper.parseFile(s"/examples/apidoc-example-union-types.json")

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/example-union-types-play-23.txt", sourceFiles.head.contents)
      }
    }
  }

  it("generates expected code for play 2.9 client and scala 3") {
    lazy val service = models.TestHelper.parseFile(s"/examples/apidoc-example-union-types-primitives.json")

    Play29Scala3ClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/example-union-types-play-29-scala-3.txt", sourceFiles.head.contents)
      }
    }
  }

  it("generates expected code for ning client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/example-union-types-ning-client.txt", sourceFiles.head.contents)
      }
    }
  }

}
