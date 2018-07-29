package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.Method
import models.TestHelper

import scala.generator.{ScalaClientMethodConfigs, ScalaClientMethodGenerator, ScalaDatatype, ScalaPrimitive, ScalaService}
import org.scalatest.{FunSpec, Matchers}

class Play2ClientGeneratorSpec extends FunSpec with Matchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play24("test.apidoc", None)

  it("errorTypeClass") {
    val service = models.TestHelper.generatorApiService
    val ssd = new ScalaService(service)
    val resource = ssd.resources.find(_.plural == "Invocations").getOrElse {
      sys.error("could not find resource with name[Invocations]")
    }
    val operation = resource.operations.find(_.method == Method.Post).get
    val errorResponse = operation.responses.find(r => models.TestHelper.responseCode(r.code) == "409").get
    errorResponse.errorClassName should be("ErrorsResponse")
    errorResponse.datatype.name should be("Seq[io.apibuilder.generator.v0.models.Error]")

    val contents = new ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package", contents)
  }

  it("only generates error wrappers for model classes (not primitives)") {
    val json = models.TestHelper.buildJson(s"""
      "imports": [],
      "headers": [],
      "info": [],
      "models": [],
      "enums": [],
      "unions": [],
      "attributes": [],

      "models": [
        {
          "name": "user",
          "plural": "user",
          "attributes": [],
          "fields": [
            { "name": "id", "type": "long", "required": true, "attributes": [] }
          ]
        }

      ],

      "resources": [
        {
          "type": "user",
          "plural": "users",
          "path": "/users",
          "attributes": [],
          "operations": [
            {
              "method": "GET",
              "path": "/:id",
              "parameters": [],
              "attributes": [],
              "responses": [
                { "code": { "integer": { "value": 200 } }, "type": "user", "headers": [] },
                { "code": { "integer": { "value": 409 } }, "type": "unit", "headers": [] }
              ]
            }
          ]
        }
      ]
    """)

    val ssd = ScalaService(models.TestHelper.service(json))
    val contents = new ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package-no-models", contents)
  }

  describe("Play 2.2.x generator basic output") {
    it("generates built-in types") {
      val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")
      Play22ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-22-built-in-types", sourceFiles.head.contents)
        }
      }
    }
  }

  describe("Play 2.3.x generator basic output") {
    it("generates built-in types") {
      val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")
      Play23ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-23-built-in-types", sourceFiles.head.contents)
        }
      }
    }
  }

  describe("Play 2.4.x generator basic output") {
    it("generates built-in types") {
      val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")
      Play24ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-24-built-in-types", sourceFiles.head.contents)
        }
      }
    }
  }

  describe("Play 2.5.x generator basic output") {
    it("generates built-in types") {
      val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")
      Play25ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-25-built-in-types", sourceFiles.head.contents)
        }
      }
    }
  }

  describe("Play 2.6.x generator basic output") {
    val service = models.TestHelper.generatorApiService
    val ssd = new ScalaService(service)
    val invocationForm = InvocationForm(service, Seq.empty, None)
    val play26Config = ScalaClientMethodConfigs.Play26("whatever", None)
    val output = Play26ClientGenerator.invoke(invocationForm).right.get

    it("is valid scala code") {
      TestHelper.assertValidScalaSourceFiles(output)
    }

    it("has non deprecated request with-methods") {
      val rawContent = output.map(_.contents).mkString("\n")
      rawContent.contains("addHttpHeaders(").shouldBe(true)
      rawContent.contains("addQueryStringParameters(").shouldBe(true)
      rawContent.contains("withHeaders(").shouldBe(false)
      rawContent.contains("withQueryString(").shouldBe(false)
    }

    it("generates built-in types") {
      val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")
      Play26ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-26-built-in-types", sourceFiles.head.contents)
        }
      }
    }
  }

/*
  it("model, enum and union use case - https://github.com/mbryzek/apidoc/issues/384") {
    val json = models.TestHelper.readFile("lib/src/test/resources/generators/play-2-union-model-enum-service.json")
    val ssd = ScalaService(models.TestHelper.service(json))
    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package-no-models", contents)
  }
 */
}
