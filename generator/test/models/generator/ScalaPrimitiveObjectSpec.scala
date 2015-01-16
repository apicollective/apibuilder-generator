package generator

import lib.Primitives
import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models._
import models.TestHelper
import org.scalatest.{ ShouldMatchers, FunSpec }

class ScalaPrimitiveObjectSpec extends FunSpec with ShouldMatchers {

  describe("for a field with an object field") {

    val baseJson = TestHelper.buildJson("""
      "models": [
        {
          "name": "content",
          "plural": "contents",
          "fields": [
            { "name": "data", "type": "%s", "required": true }
          ]
        }
      ]
    """)

    def service(typeString: String): Service = {
      TestHelper.service(baseJson.format(typeString))
    }

    def ssd(typeString: String): ScalaService = {
      ScalaService(service(typeString))
    }

    def dataField(typeString: String): ScalaField = {
      ssd(typeString).models.head.fields.head
    }

    it("singleton object") {
      dataField("object").`type`.label should be("object")
    }

    it("list object") {
      dataField("[object]").`type`.label should be("[object]")
    }

    it("map object") {
      dataField("map[object]").`type`.label should be("map[object]")
    }

    describe("generates valid case classes") {

      it("singleton") {
        val code = ScalaCaseClasses.invoke(InvocationForm(service("object")), addHeader = false)
        TestHelper.assertEqualsFile("test/resources/generators/scala-primitive-object-singleton.txt", code)
      }

      it("list") {
        val code = ScalaCaseClasses.invoke(InvocationForm(service("[object]")), addHeader = false)
        TestHelper.assertEqualsFile("test/resources/generators/scala-primitive-object-list.txt", code)
      }

      it("map") {
        val code = ScalaCaseClasses.invoke(InvocationForm(service("map[object]")), addHeader = false)
        TestHelper.assertEqualsFile("test/resources/generators/scala-primitive-object-map.txt", code)
      }

    }

  }

  describe("for a response with an object field") {

    val contentModel = """
        {
          "name": "content",
          "plural": "contents",
          "fields": [
            { "name": "id", "type": "long", "required": true }
          ]
        }
    """

    val baseJson = TestHelper.buildJson(s"""
      "models": [$contentModel],

      "resources": [
        {
          "model": $contentModel,
          "operations": [
            {
              "method": "GET",
              "path": "/contents/data",
              "responses": [
                { "code": 200, "type": "%s" }
              ]
            }
          ]
        }
      ]
    """)

    def service(typeString: String): Service = {
      TestHelper.service(baseJson.format(typeString))
    }

    def ssd(typeString: String): ScalaService = {
      ScalaService(service(typeString))
    }

    def operation(typeString: String): ScalaOperation = {
      ssd(typeString).resources.head.operations.head
    }

    def response(typeString: String): ScalaResponse = {
      operation(typeString).responses.head
    }

    it("singleton object") {
      response("object").`type`.label should be("object")
    }

    it("list object") {
      response("[object]").`type`.label should be("[object]")
    }

    it("map object") {
      response("map[object]").`type`.label should be("map[object]")
    }

    describe("generates valid response code") {


      it("singleton") {
        val generator = new ScalaClientMethodGenerator(ScalaClientMethodConfigs.Play23, ssd("object"))
        TestHelper.assertEqualsFile("test/resources/generators/scala-primitive-object-response-singleton.txt", generator.objects)
      }

      it("list") {
        val generator = new ScalaClientMethodGenerator(ScalaClientMethodConfigs.Play23, ssd("[object]"))
        TestHelper.assertEqualsFile("test/resources/generators/scala-primitive-object-response-list.txt", generator.objects)
      }

      it("map") {
        val generator = new ScalaClientMethodGenerator(ScalaClientMethodConfigs.Play23, ssd("map[object]"))
        TestHelper.assertEqualsFile("test/resources/generators/scala-primitive-object-response-map.txt", generator.objects)
      }

    }

  }

}
