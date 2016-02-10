package scala.generator

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models._
import models.TestHelper
import org.scalatest.{ ShouldMatchers, FunSpec }

class ScalaPrimitiveObjectSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play23("test.apidoc", None)

  describe("for a field with an object field") {

    val baseJson = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "unions": [],
      "resources": [],

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
      models.TestHelper.service(baseJson.format(typeString))
    }

    def ssd(typeString: String): ScalaService = {
      ScalaService(service(typeString))
    }

    def dataField(typeString: String): ScalaField = {
      ssd(typeString).models.head.fields.head
    }

    it("singleton object") {
      dataField("object").`type`.name should be("object")
    }

    it("list object") {
      dataField("[object]").`type`.name should be("[object]")
    }

    it("map object") {
      dataField("map[object]").`type`.name should be("map[object]")
    }

    describe("generates valid case classes") {

      it("singleton") {
        ScalaCaseClasses.invoke(InvocationForm(service("object")), addHeader = false) match {
          case Left(errors) => fail(errors.mkString(", "))
          case Right(sourceFiles) => {
            sourceFiles.size shouldBe 1
            models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-singleton.txt", sourceFiles.head.contents)
          }
        }
      }

      it("list") {
        ScalaCaseClasses.invoke(InvocationForm(service("[object]")), addHeader = false) match {
          case Left(errors) => fail(errors.mkString(", "))
          case Right(sourceFiles) => {
            sourceFiles.size shouldBe 1
            models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-list.txt", sourceFiles.head.contents)
          }
        }
      }

      it("map") {
        ScalaCaseClasses.invoke(InvocationForm(service("map[object]")), addHeader = false) match {
          case Left(errors) => fail(errors.mkString(", "))
          case Right(sourceFiles) => {
            sourceFiles.size shouldBe 1
            models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-map.txt", sourceFiles.head.contents)
          }
        }
      }

    }

  }

  describe("for a response with an object field") {

    val baseJson = models.TestHelper.buildJson(s"""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "unions": [],

      "models": [
        {
          "name": "content",
          "plural": "contents",
          "fields": [
            { "name": "id", "type": "long", "required": true }
          ]
        }
      ],

      "resources": [
        {
          "type": "content",
          "plural": "contents",

          "operations": [
            {
              "method": "GET",
              "path": "/contents/data",
              "parameters": [],
              "responses": [
                { "code": { "integer": { "value": 200 } }, "type": "%s" }
              ]
            }
          ]
        }
      ]
    """)

    def service(typeString: String): Service = {
      models.TestHelper.service(baseJson.format(typeString))
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
      response("object").`type`.name should be("object")
    }

    it("list object") {
      response("[object]").`type`.name should be("[object]")
    }

    it("map object") {
      response("map[object]").`type`.name should be("map[object]")
    }

    describe("generates valid response code") {


      it("singleton") {
        val generator = new ScalaClientMethodGenerator(clientMethodConfig, ssd("object"))
        models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-response-singleton.txt", generator.objects)
      }

      it("list") {
        val generator = new ScalaClientMethodGenerator(clientMethodConfig, ssd("[object]"))
        models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-response-list.txt", generator.objects)
      }

      it("map") {
        val generator = new ScalaClientMethodGenerator(clientMethodConfig, ssd("map[object]"))
        models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-response-map.txt", generator.objects)
      }

    }

  }

}
