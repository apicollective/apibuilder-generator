package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
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

    it("should not call implicit methods") {
      val json = models.TestHelper.buildJson("""
        "imports": [],
        "headers": [],
        "info": [],
        "unions": [],
        "enums": [],
        "resources": [],
        "attributes": [],
        "models": [
          {
            "name": "foo",
            "plural": "foos",
            "attributes": [],
            "fields": [
              { "name": "bar", "type": "foo", "required": true, "attributes": [] },
              { "name": "baz", "type": "[foo]", "required": true, "attributes": [] }
            ]
          }
        ]
      """)

      val ssd = ScalaService(models.TestHelper.service(json))
      val model = ssd.models.head
      val reader = Play2Json(ssd).fieldReaders(model)

      assert(reader contains """(__ \ "bar").lazyRead(play.api.libs.json.Reads.of[test.apidoc.apidoctest.v0.models.Foo])""")
      assert(reader contains """(__ \ "baz").lazyRead(play.api.libs.json.Reads.of[Seq[test.apidoc.apidoctest.v0.models.Foo]])""")
    }
  }
}
