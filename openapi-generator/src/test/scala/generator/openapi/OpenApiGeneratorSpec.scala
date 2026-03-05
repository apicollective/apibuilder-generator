package generator.openapi

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.Service
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

class OpenApiGeneratorSpec extends AnyWordSpec with Matchers {

  private lazy val petstoreService: Service = {
    val stream = getClass.getResourceAsStream("/petstore-apibuilder.json")
    val json = scala.io.Source.fromInputStream(stream).mkString
    stream.close()
    Json.parse(json).as[Service]
  }

  "OpenApiJsonGenerator" should {

    "generate valid JSON with openapi 3.0.3" in {
      val form = InvocationForm(petstoreService)
      val result = OpenApiJsonGenerator.invoke(form)
      result.isRight mustBe true
      val files = result.toOption.get
      files must have size 1
      val contents = files.head.contents
      contents must include("\"openapi\"")
      contents must include("\"3.0.3\"")
      contents must include("\"petstore\"")
    }

    "include paths in JSON output" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiJsonGenerator.invoke(form).toOption.get
      val contents = files.head.contents
      contents must include("/pets")
      contents must include("/pets/{id}")
    }

    "include component schemas in JSON output" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiJsonGenerator.invoke(form).toOption.get
      val contents = files.head.contents
      contents must include("schemas")
      // Models should be present as PascalCase sanitized names
      contents must include("Pet")
      contents must include("Error")
    }

    "include security schemes from headers" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiJsonGenerator.invoke(form).toOption.get
      val contents = files.head.contents
      contents must include("bearerAuth")
      contents must include("bearer")
    }

    "generate file with .json extension" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiJsonGenerator.invoke(form).toOption.get
      files.head.name must endWith(".json")
    }

    "be parseable as JSON" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiJsonGenerator.invoke(form).toOption.get
      val parsed = Json.parse(files.head.contents)
      (parsed \ "openapi").as[String] mustBe "3.0.3"
    }

    "not contain x-amazon-apigateway extensions without attribute" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiJsonGenerator.invoke(form).toOption.get
      val contents = files.head.contents
      contents must not include "x-amazon-apigateway"
    }

    "contain x-amazon-apigateway-integration with aws_api_gateway attribute" in {
      val attr = Attribute(
        name = "aws_api_gateway",
        value = """{"integration": {"type": "http_proxy", "domain": "internal.flow.io"}}""",
      )
      val form = InvocationForm(petstoreService, attributes = Seq(attr))
      val result = OpenApiJsonGenerator.invoke(form)
      result.isRight mustBe true
      val contents = result.toOption.get.head.contents
      contents must include("x-amazon-apigateway-integration")
      contents must include("x-amazon-apigateway-request-validators")
    }

    "contain x-amazon-apigateway-authorizer with authorizer config" in {
      val attr = Attribute(
        name = "aws_api_gateway",
        value = """{"integration": {"type": "http_proxy", "domain": "d"}, "authorizer": {"name": "myAuth", "uri": "arn:aws:lambda:us-east-1:123:function:auth"}}""",
      )
      val form = InvocationForm(petstoreService, attributes = Seq(attr))
      val result = OpenApiJsonGenerator.invoke(form)
      result.isRight mustBe true
      val contents = result.toOption.get.head.contents
      contents must include("x-amazon-apigateway-authorizer")
      contents must include("myAuth")
    }

    "return Left for invalid aws_api_gateway attribute JSON" in {
      val attr = Attribute(
        name = "aws_api_gateway",
        value = "not valid json",
      )
      val form = InvocationForm(petstoreService, attributes = Seq(attr))
      val result = OpenApiJsonGenerator.invoke(form)
      result.isLeft mustBe true
      result.left.toOption.get.head must include("aws_api_gateway")
    }
  }

  "OpenApiYamlGenerator" should {

    "generate valid YAML with openapi version" in {
      val form = InvocationForm(petstoreService)
      val result = OpenApiYamlGenerator.invoke(form)
      result.isRight mustBe true
      val files = result.toOption.get
      files must have size 1
      val contents = files.head.contents
      contents must include("openapi")
      contents must include("3.0.3")
      contents must include("petstore")
    }

    "include paths in YAML output" in {
      val form = InvocationForm(petstoreService)
      val files = OpenApiYamlGenerator.invoke(form).toOption.get
      val contents = files.head.contents
      contents must include("/pets")
      contents must include("/pets/{id}")
    }
  }

  "OpenApiConverter" should {

    "set server from baseUrl" in {
      val openApi = OpenApiConverter.convert(petstoreService, Nil)
      openApi.servers must have size 1
      openApi.servers.head.url mustBe "https://petstore.example.com/v1"
    }

    "produce paths with correct operations" in {
      val openApi = OpenApiConverter.convert(petstoreService, Nil)
      val pathItems = openApi.paths.pathItems
      pathItems must contain key "/pets"
      pathItems must contain key "/pets/{id}"
      pathItems("/pets").get must be(defined)
      pathItems("/pets").post must be(defined)
      pathItems("/pets/{id}").get must be(defined)
      pathItems("/pets/{id}").delete must be(defined)
    }
  }
}
