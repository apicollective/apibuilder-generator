package scala.models

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import org.scalatest.FunSpec

import scala.generator.ScalaService

class RecursiveJsonSpec extends FunSpec {
  val service = ScalaService(models.TestHelper.parseFile("/examples/recursive-types.json"))
  val foo = service.models.find(_.name == "Foo").get
  val bar = service.models.find(_.name == "Bar").get

  describe("recursive models") {

    it("readers") {
      assert(Play2Json(service).readers(foo) contains "lazyReadNullable(") // nullable since the_bar is not required
      assert(Play2Json(service).readers(bar) contains "lazyRead(")
    }
  }

  describe("imports") {
    Play2StandaloneModelsJson.invoke(InvocationForm(service.service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(files) =>
    }
  }
}
