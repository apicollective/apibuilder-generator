package scala.generator

import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models._
import models.TestHelper
import org.scalatest.{ Matchers, FunSpec }

class ScalaPrimitiveObjectSpec extends FunSpec with Matchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play23("test.apidoc", None)

  describe("for a field with an object field") {

    val baseJson = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "unions": [],
      "resources": [],
      "attributes": [],

      "models": [
        {
          "name": "content",
          "plural": "contents",
          "attributes": [],
          "fields": [
            { "name": "data", "type": "%s", "required": true, "attributes": [] }
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
            models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-singleton", sourceFiles.head.contents)
          }
        }
      }

      it("list") {
        ScalaCaseClasses.invoke(InvocationForm(service("[object]")), addHeader = false) match {
          case Left(errors) => fail(errors.mkString(", "))
          case Right(sourceFiles) => {
            sourceFiles.size shouldBe 1
            models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-list", sourceFiles.head.contents)
          }
        }
      }

      it("map") {
        ScalaCaseClasses.invoke(InvocationForm(service("map[object]")), addHeader = false) match {
          case Left(errors) => fail(errors.mkString(", "))
          case Right(sourceFiles) => {
            sourceFiles.size shouldBe 1
            models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-map", sourceFiles.head.contents)
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
      "attributes": [],

      "models": [
        {
          "name": "content",
          "plural": "contents",
          "attributes": [],
          "fields": [
            { "name": "id", "type": "long", "required": true, "attributes": [] }
          ]
        }
      ],

      "resources": [
        {
          "type": "content",
          "plural": "contents",
          "attributes": [],

          "operations": [
            {
              "method": "GET",
              "path": "/contents/data",
              "parameters": [],
              "attributes": [],
              "responses": [
                { "code": { "integer": { "value": 200 } }, "type": "%s", "headers": [] }
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
        models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-response-singleton", generator.objects)
      }

      it("list") {
        val generator = new ScalaClientMethodGenerator(clientMethodConfig, ssd("[object]"))
        models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-response-list", generator.objects)
      }

      it("map") {
        val generator = new ScalaClientMethodGenerator(clientMethodConfig, ssd("map[object]"))
        models.TestHelper.assertEqualsFile("/generators/scala-primitive-object-response-map", generator.objects)
      }

    }

  }

}
