package scala.models

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import io.apibuilder.spec.v0.models.Method
import models.TestHelper
import models.TestHelper.assertValidScalaSourceCode

import scala.generator.{ScalaClientMethodConfigs, ScalaClientMethodGenerator, ScalaService}
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
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package.txt", contents)
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
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package-no-models.txt", contents)
  }

  describe("Play 2.2.x generator basic output") {
    it("generates built-in types") {
      val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")
      Play22ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-22-built-in-types.txt", sourceFiles.head.contents)
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
          models.TestHelper.assertEqualsFile("/generators/play-23-built-in-types.txt", sourceFiles.head.contents)
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
          models.TestHelper.assertEqualsFile("/generators/play-24-built-in-types.txt", sourceFiles.head.contents)
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
          models.TestHelper.assertEqualsFile("/generators/play-25-built-in-types.txt", sourceFiles.head.contents)
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
          models.TestHelper.assertEqualsFile("/generators/play-26-built-in-types.txt", sourceFiles.head.contents)
        }
      }
    }

    it("generates apidoc-api") {
      val form = new InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Play26ClientGenerator.invoke(form)
      files.size shouldBe 1
      files(0).name shouldBe "BryzekApidocApiV0Client.scala"
      assertValidScalaSourceCode(files(0).contents)
      models.TestHelper.assertEqualsFile(s"/generators/play-26-apidoc-api.txt", files(0).contents)
    }
  }

  describe("Play 2.7.x generator basic output") {
    val service = models.TestHelper.generatorApiService
    val ssd = new ScalaService(service)
    val invocationForm = InvocationForm(service, Seq.empty, None)
    val play27Config = ScalaClientMethodConfigs.Play27("whatever", None)
    val output = Play27ClientGenerator.invoke(invocationForm).right.get

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
      Play27ClientGenerator.invoke(InvocationForm(service = service)) match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile("/generators/play-27-built-in-types.txt", sourceFiles.head.contents)
        }
      }
    }

    it("generates apidoc-api") {
      val form = new InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Play27ClientGenerator.invoke(form)
      files.size shouldBe 1
      files(0).name shouldBe "BryzekApidocApiV0Client.scala"
      assertValidScalaSourceCode(files(0).contents)
      models.TestHelper.assertEqualsFile(s"/generators/play-27-apidoc-api.txt", files(0).contents)
    }
  }

  describe("Play 2.7.x generator joda  vs. java time") {
    it("generates date-time with joda") {
      val form = new InvocationForm(
        models.TestHelper.parseFile("/examples/date-time-types.json"),
        Seq.empty,
        None
      )
      val Right(files) = Play27ClientGenerator.invoke(form)
      files.size shouldBe 1
      files(0).name shouldBe "ApibuilderTimeTypesV0Client.scala"
      assertValidScalaSourceCode(files(0).contents)
      models.TestHelper.assertEqualsFile(s"/generators/play-27-joda-date-time.txt", files(0).contents)
    }

    it("generates date-time with java.time") {
      val form = new InvocationForm(
        models.TestHelper.parseFile("/examples/date-time-types.json"),
        Seq(Attribute("time", "java")),
        None
      )
      val Right(files) = Play27ClientGenerator.invoke(form)
      files.size shouldBe 1
      files(0).name shouldBe "ApibuilderTimeTypesV0Client.scala"
      assertValidScalaSourceCode(files(0).contents)
      models.TestHelper.assertEqualsFile(s"/generators/play-27-java-date-time.txt", files(0).contents)
    }
  }

/*
  it("model, enum and union use case - https://github.com/mbryzek/apidoc/issues/384") {
    val json = models.TestHelper.readFile("lib/src/test/resources/generators/play-2-union-model-enum-service.json")
    val ssd = ScalaService(models.TestHelper.service(json))
    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package-no-models.txt", contents)
  }
 */
}
