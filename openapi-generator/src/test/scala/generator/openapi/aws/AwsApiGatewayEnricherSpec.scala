package generator.openapi.aws

import io.apibuilder.spec.v0.{models => ab}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

class AwsApiGatewayEnricherSpec extends AnyWordSpec with Matchers {

  private val testService = ab.Service(
    apidoc = Some(ab.Apidoc(version = "0.0.1")),
    name = "test-service",
    organization = ab.Organization(key = "test"),
    application = ab.Application(key = "test-service"),
    namespace = "test.service",
    version = "1.0.0",
    info = ab.Info(license = None, contact = None),
    headers = Nil,
    imports = Nil,
    enums = Nil,
    interfaces = Nil,
    unions = Nil,
    models = Nil,
    resources = Nil,
    attributes = Nil,
    annotations = Nil,
  )

  private val testOperation: Operation = Operation(
    operationId = Some("listPets"),
  )

  private val baseOpenApi = OpenAPI(
    openapi = "3.0.3",
    info = Info(title = "test", version = "1.0.0"),
    paths = Paths(
      pathItems = ListMap(
        "/pets" -> PathItem(get = Some(testOperation)),
        "/pets/{id}" -> PathItem(get = Some(testOperation.copy(operationId = Some("getPet")))),
      ),
    ),
  )

  "AwsApiGatewayEnricher" should {

    "add x-amazon-apigateway-request-validators at top level" in {
      val config = AwsApiGatewayConfig(
        integration = AwsApiGatewayIntegration(`type` = "http_proxy", domain = "internal.flow.io"),
      )
      val enriched = AwsApiGatewayEnricher.enrich(baseOpenApi, config, testService)
      enriched.extensions must contain key "x-amazon-apigateway-request-validators"
    }

    "add http_proxy integration with correct URI" in {
      val config = AwsApiGatewayConfig(
        integration = AwsApiGatewayIntegration(
          `type` = "http_proxy",
          domain = "internal.flow.io",
          protocol = "https",
        ),
      )
      val enriched = AwsApiGatewayEnricher.enrich(baseOpenApi, config, testService)
      val petsGet = enriched.paths.pathItems("/pets").get.get
      val ext = petsGet.extensions("x-amazon-apigateway-integration")
      ext.value.toString must include("https://test-service.internal.flow.io/pets")
      ext.value.toString must include("http_proxy")
    }

    "produce mock integration with request template" in {
      val config = AwsApiGatewayConfig(
        integration = AwsApiGatewayIntegration(`type` = "mock"),
      )
      val enriched = AwsApiGatewayEnricher.enrich(baseOpenApi, config, testService)
      val petsGet = enriched.paths.pathItems("/pets").get.get
      val ext = petsGet.extensions("x-amazon-apigateway-integration")
      ext.value.toString must include("\"type\": \"mock\"")
      ext.value.toString must include("requestTemplates")
      ext.value.toString must include("statusCode")
    }

    "forward path variables as request parameters" in {
      val config = AwsApiGatewayConfig(
        integration = AwsApiGatewayIntegration(`type` = "http_proxy", domain = "internal.flow.io"),
      )
      val enriched = AwsApiGatewayEnricher.enrich(baseOpenApi, config, testService)
      val petGet = enriched.paths.pathItems("/pets/{id}").get.get
      val ext = petGet.extensions("x-amazon-apigateway-integration")
      ext.value.toString must include("integration.request.path.id")
      ext.value.toString must include("method.request.path.id")
    }

    "add request validator to each operation" in {
      val config = AwsApiGatewayConfig(
        integration = AwsApiGatewayIntegration(`type` = "http_proxy", domain = "internal.flow.io"),
      )
      val enriched = AwsApiGatewayEnricher.enrich(baseOpenApi, config, testService)
      val petsGet = enriched.paths.pathItems("/pets").get.get
      petsGet.extensions must contain key "x-amazon-apigateway-request-validator"
    }

    "add authorizer as security scheme with x-amazon-apigateway-authorizer" in {
      val config = AwsApiGatewayConfig(
        integration = AwsApiGatewayIntegration(`type` = "http_proxy", domain = "internal.flow.io"),
        authorizer = Some(AwsApiGatewayAuthorizer(
          name = "myAuth",
          uri = "arn:aws:lambda:us-east-1:123:function:auth",
        )),
      )
      val enriched = AwsApiGatewayEnricher.enrich(baseOpenApi, config, testService)
      val components = enriched.components.get
      components.securitySchemes must contain key "myAuth"
      val scheme = components.securitySchemes("myAuth").toOption.get
      scheme.extensions must contain key "x-amazon-apigateway-authorizer"
      scheme.extensions must contain key "x-amazon-apigateway-authtype"
      enriched.security.flatMap(_.keys) must contain("myAuth")
    }
  }

  "AwsApiGatewayConfig.fromJson" should {

    "parse valid JSON config" in {
      val json = """{"integration": {"type": "http_proxy", "domain": "internal.flow.io"}}"""
      val result = AwsApiGatewayConfig.fromJson(json)
      result.isRight mustBe true
      result.toOption.get.integration.`type` mustBe "http_proxy"
      result.toOption.get.integration.domain mustBe "internal.flow.io"
    }

    "parse config with authorizer" in {
      val json = """{"integration": {"type": "http_proxy", "domain": "d"}, "authorizer": {"name": "a", "uri": "arn:123"}}"""
      val result = AwsApiGatewayConfig.fromJson(json)
      result.isRight mustBe true
      result.toOption.get.authorizer.get.name mustBe "a"
    }

    "return Left for invalid JSON" in {
      val result = AwsApiGatewayConfig.fromJson("not json")
      result.isLeft mustBe true
    }

    "return Left for missing required fields" in {
      val result = AwsApiGatewayConfig.fromJson("""{"integration": {}}""")
      result.isLeft mustBe true
    }
  }
}
