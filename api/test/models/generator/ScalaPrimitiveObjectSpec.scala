package generator

import lib.Primitives
import com.gilt.apidocgenerator.models._
import models.TestHelper
import org.scalatest.{ ShouldMatchers, FunSpec }

class ScalaPrimitiveObjectSpec extends FunSpec with ShouldMatchers {

  describe("for a field with an object field") {

    val baseJson = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc Test",

      "models": {
        "content": {
          "fields": [
            { "name": "data", "type": "%s" }
          ]
        }
      }
    }
    """

    def service(typeString: String): Service = {
      TestHelper.service(baseJson.format(typeString))
    }

    def ssd(typeString: String): ScalaService = {
      ScalaService(service(typeString))
    }

    def dataField(typeString: String): ScalaField = {
      ssd(typeString).models.values.head.fields.head
    }

    it("singleton object") {
      dataField("object").`type` should be("object")
    }

    it("list object") {
      dataField("[object]").`type` should be("[object]")
    }

    it("map object") {
      dataField("map[object]").`type` should be("map[object]")
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

    val baseJson = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc Test",

      "models": {
        "content": {
          "fields": [
            { "name": "id", "type": "long" }
          ]
        }
      },

      "resources": {
        "content": {
          "operations": [
            {
              "method": "GET",
              "path": "/data",
              "responses": {
                "200": { "type": "%s" }
              }
            }
          ]
        }
      }

    }
    """

    def service(typeString: String): Service = {
      TestHelper.service(baseJson.format(typeString))
    }

    def ssd(typeString: String): ScalaService = {
      ScalaService(service(typeString))
    }

    def operation(typeString: String): ScalaOperation = {
      ssd(typeString).resources.values.head.operations.head
    }

    def response(typeString: String): ScalaResponse = {
      operation(typeString).responses.head
    }

    it("singleton object") {
      response("object").`type` should be("object")
    }

    it("list object") {
      response("[object]").`type` should be("[object]")
    }

    it("map object") {
      response("map[object]").`type` should be("map[object]")
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
