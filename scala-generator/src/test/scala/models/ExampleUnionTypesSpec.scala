package scala.models

import com.gilt.apidoc.generator.v0.models.InvocationForm
import ning.Ning18ClientGenerator
import org.scalatest.{ShouldMatchers, FunSpec}

class ExampleUnionTypesSpec extends FunSpec with ShouldMatchers {

  private lazy val service = models.TestHelper.parseFile(s"/examples/union-types-service.json")

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        models.TestHelper.assertEqualsFile("/example-union-types-play-23.txt", code)
      }
    }
  }

  it("generates expected code for ning client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        models.TestHelper.assertEqualsFile("/example-union-types-ning-client.txt", code)
      }
    }
  }

}
