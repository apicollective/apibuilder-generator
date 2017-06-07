package scala.models

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import org.scalatest.FunSpec

import scala.generator.ScalaService

class RecursiveJsonSpec extends FunSpec {
  val service = ScalaService(models.TestHelper.parseFile("/examples/recursive-types.json"))

  describe("recursive models") {
    it("should have lazy readers") {
      val foo = service.models.find(_.name == "Foo").get
      val bar = service.models.find(_.name == "Bar").get
      assert(Play2Json(service).readers(foo) contains "lazyReadNullable(") // nullable since the_bar is not required
      assert(Play2Json(service).readers(bar) contains "lazyRead(")
    }

    it("should work with imports") {
      Play2StandaloneModelsJson.invoke(InvocationForm(service.service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(files) => println(files.head.contents)
      }
    }

    it("should work with unions") {
      val gadget = service.models.find(_.name == "Gadget").get
      val widget = service.models.find(_.name == "Widget").get
      assert(Play2Json(service).readers(gadget) contains "lazyRead(")
      assert(Play2Json(service).readers(widget) contains "lazyRead(")
    }
  }
}
