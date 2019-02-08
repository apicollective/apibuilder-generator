package generator

import examples.ExampleJson
import generator.Utils.Description
import io.apibuilder.spec.v0.models._
import io.flow.postman.collection.v210.v0.{models => postman}
import org.scalatest.{Assertion, Matchers, WordSpec}
import play.api.libs.json.{JsString, JsValue, Json}

class PostmanItemBuilderSpec extends WordSpec with Matchers {

  "PostmanItemBuilder" should {

    "build a simple GET request" in new TestContext {
      performTest
    }

    "build a POST request with example payload inside" in new TestContext {
      override val method = Method.Post

      override val testOperation: Operation = super.testOperation.copy(body = Some(body))

      performTest { postmanItem =>
        postmanItem.request.body.isDefined shouldEqual true
        postmanItem.request.body.get.mode shouldEqual Some(postman.BodyMode.Raw)
        Json.parse(postmanItem.request.body.get.raw.get) shouldEqual exampleJson
        postmanItem.request.header shouldEqual Some(Seq(contentTypeHeader))
      }
    }

    "build a request with a header defined on service level" in new TestContext {
      override def serviceSpecificHeaders: Seq[postman.Header] = Seq(postmanHeader)

      performTest { postmanItem =>
        postmanItem.request.header shouldEqual Some(Seq(postmanHeader))
      }
    }

    "build a request with a header defined on operation level" in new TestContext {
      override val testOperation: Operation = super.testOperation.copy(
        parameters = Seq(operationHeaderParam)
      )

      performTest { postmanItem =>
        postmanItem.request.header shouldEqual Some(Seq(postmanHeader))
      }
    }

    "build a request with a path param" in new TestContext {
      override val testOperation: Operation = super.testOperation.copy(
        parameters = Seq(operationPathParam)
      )

      performTest { postmanItem =>
        postmanItem.request.url.get.variable shouldEqual Some(Seq(postmanUrlPathVariable))
      }
    }

    "build a request with a query param" in new TestContext {
      override val testOperation: Operation = super.testOperation.copy(
        parameters = Seq(operationQueryParam)
      )

      performTest { postmanItem =>
        postmanItem.request.url.get.query shouldEqual Some(Seq(postmanUrlQuery))
      }
    }

    "generate a simple parameter description when it's not specified" in new TestContext {
      override val testOperation: Operation = super.testOperation.copy(
        parameters = Seq(operationHeaderParam.copy(description = None))
      )

      performTest { postmanItem =>
        postmanItem.request.header shouldEqual Some(Seq(
          postmanHeader.copy(description = Some(Description("Type: string  | Required: true")))
        ))
      }
    }

    "build example responses when they're specified in an operation" in new TestContext {
      override val testOperation: Operation = super.testOperation.copy(
        responses = Seq(
          Response(code = ResponseCodeInt(200), `type` = mockedType),
          Response(code = ResponseCodeInt(400), `type` = mockedType)
        )
      )

      performTest { postmanItem =>
        postmanItem.response shouldEqual Some(Seq(
          postmanExampleResponse.copy(
            name = Some(s"Example 200 - $mockedType"),
            originalRequest = Some(postmanItem.request)
          ),
          postmanExampleResponse.copy(
            name = Some(s"Example 400 - $mockedType"),
            code = Some(400),
            originalRequest = Some(postmanItem.request)
          )
        ))
      }
    }

    "build an example response with no body when operation response has body type 'unit'" in new TestContext {
      override val testOperation: Operation = super.testOperation.copy(
        responses = Seq(
          Response(code = ResponseCodeInt(404), `type` = "unit")
        )
      )

      performTest { postmanItem =>
        postmanItem.response shouldEqual Some(Seq(
          postmanExampleResponse.copy(
            name = Some(s"Example 404 - unit"),
            code = Some(404),
            body = None,
            originalRequest = Some(postmanItem.request)
          )
        ))
      }
    }

    "build a POST request without example payload when Example Provider fails" in new TestContext {
      override val method = Method.Post

      override val exampleProvider = new ExampleJson(null, null) {
        override def sample(typ: String, subTyp: Option[String]): Option[JsValue] = {
          None
        }
      }

      override val testOperation: Operation = super.testOperation.copy(body = Some(body))

      performTest { postmanItem =>
        postmanItem.request.body.isDefined shouldEqual false
        postmanItem.request.header shouldEqual Some(Seq(contentTypeHeader))
      }
    }

  }

  trait TestContext {

    val description = "Very elaborate endpoint description"
    val baseUrl = "http://test.api.com"
    val path = "/resource/operation/all"
    val exampleJson = Json.obj("example" -> JsString("value"))

    def exampleProvider: ExampleJson = new ExampleJson(null, null) {
      override def sample(typ: String, subTyp: Option[String]): Option[JsValue] = {
        Some(exampleJson)
      }
    }

    val mockedType = "irrelevant because of mocked example provider"
    val body = Body(`type` = mockedType)

    val contentTypeHeader = postman.Header("Content-Type", "application/json", description = Some(Description("Required to send JSON body")))

    val paramName = "X-Special-Param"
    val paramValue = "Value"
    val paramDescription = "Parameter description"

    val operationHeaderParam = Parameter(
      name = paramName,
      description = Some(paramDescription),
      `type` = "string",
      location = ParameterLocation.Header,
      required = true,
      example = Some(paramValue))

    val postmanHeader = postman.Header(paramName, paramValue, description = Some(Description(paramDescription)))

    val operationPathParam = operationHeaderParam.copy(location = ParameterLocation.Path)
    val postmanUrlPathVariable = postman.Variable(key = Some(paramName), value = Some(paramValue),description = Some(Description(paramDescription)), disabled = Some(false))

    val operationQueryParam = operationHeaderParam.copy(location = ParameterLocation.Query)
    val postmanUrlQuery = postman.QueryParam(key = Some(paramName), value = Some(paramValue), description = Some(Description(paramDescription)), disabled = Some(false))

    val postmanExampleResponse = postman.Response(
      id = None,
      name = None,
      originalRequest = None,
      header = None,
      body = Some(Json.prettyPrint(exampleJson)),
      status = None,
      code = Some(200)
    )

    def method: Method = Method.Get

    def testOperation: Operation = Operation(
      method = method,
      path = path,
      body = None,
      description = Some(description)
    )

    def serviceSpecificHeaders: Seq[postman.Header] = Seq.empty

    def buildPostmanItem: postman.Item = PostmanItemBuilder.build(baseUrl, testOperation, serviceSpecificHeaders, exampleProvider, None)

    def commonAssertions(postmanItem: postman.Item): Assertion = {
      postmanItem.name shouldEqual Some(s"$method $path")
      postmanItem.description shouldEqual Some(Description(description))
      postmanItem.request.url.get.raw shouldEqual /* TODO: after fix, it should be baseUrl, not a variable */ Some("{{BASE_URL}}" + path)
      postmanItem.event shouldEqual None
    }

    def performTest: Assertion = {
      val postmanItem = buildPostmanItem
      commonAssertions(postmanItem)
    }

    def performTest(extraAssertions: postman.Item => Assertion): Assertion = {
      val postmanItem = buildPostmanItem
      commonAssertions(postmanItem)
      extraAssertions(postmanItem)
    }

  }

}

