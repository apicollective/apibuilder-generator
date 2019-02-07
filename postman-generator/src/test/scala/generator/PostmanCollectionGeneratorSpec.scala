package generator

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import org.scalatest.{Assertion, WordSpec}
import play.api.libs.json.{JsArray, JsDefined, JsValue, Json}

import scala.util.Try

class PostmanCollectionGeneratorSpec extends WordSpec {

  import models.TestHelper._

  "PostmanCollectionGenerator" should {

    "return an error when invocation form contains a service with declared, but not provided imports" in {
      val invocationForm = InvocationForm(referenceWithImportsApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isLeft shouldEqual true
      result.left.get shouldEqual Seq("Service imports need to be resolved before generating Postman Collection. However InvocationForm.importedServices is empty")
    }

    // TODO: instantiate a trivial service
    "return a generated Postman Collection for a trivial service" in {
      true shouldEqual true
    }

    "return a generated Postman Collection for a reference service without imports" in {
      val invocationForm = InvocationForm(referenceApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collectionJson =>
        val postmanFolders = (collectionJson \ "item").as[JsArray].value
        val folderNames = postmanFolders.map(js => (js \ "name").as[String])
        folderNames shouldEqual List("Setup", "echoes", "groups", "members", "organizations", "users", "Cleanup")

        val membersFolder = postmanFolders.find(js => (js \ "name").as[String] == "members").get
        val memberEndpoints = (membersFolder \ "item").as[JsArray].value
        val bulkCreateMemberEndpoint = memberEndpoints.find(js => (js \ "name").as[String].contains("/members/:organization/members_bulk"))
          .getOrElse(fail("reference service does not contain POST members_bulk operation"))
        val bulkCreateMemberRequest = bulkCreateMemberEndpoint \ "request"

        val bulkCreateMemberBody = bulkCreateMemberRequest \ "body"
        (bulkCreateMemberBody \ "mode").as[String] shouldEqual "raw"
        val bulkCreateMemberPayloadString = (bulkCreateMemberBody \ "raw").as[String]
        Try(Json.parse(bulkCreateMemberPayloadString)).isSuccess shouldEqual true

        val bulkCreateMemberRawUrl = (bulkCreateMemberRequest \ "url" \ "raw").as[String]
        bulkCreateMemberRawUrl shouldEqual "{{BASE_URL}}/members/:organization/members_bulk"

        val bulkCreateMemberUrlVariable = bulkCreateMemberRequest \ "url" \ "variable"
        bulkCreateMemberUrlVariable shouldEqual JsDefined(Json.parse(
          """
            |[{
            |    "key": "organization",
            |    "value": "{{ORGANIZATION}}",
            |    "description": {
            |        "content":"Type: uuid  | Required: true"
            |     },
            |    "disabled": false
            |}]
          """.stripMargin))

        (bulkCreateMemberRequest \ "method").as[String] shouldEqual "POST"
        val bulkCreateMemberHeader = (bulkCreateMemberRequest \ "header").as[JsArray].value.head
        (bulkCreateMemberHeader \ "key").as[String] shouldEqual "Content-Type"
        (bulkCreateMemberHeader \ "value").as[String] shouldEqual "application/json"
      }
    }

    // TODO: perform a real testing with imported services
    "return a generated Postman Collection for a service with imports" in {
      val invocationForm = InvocationForm(referenceApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isRight shouldEqual true
    }

  }

  private def assertResultCollectionJson(result: Either[Seq[String], Seq[File]])(jsonAssertion: JsValue => Assertion): Assertion = {
    result.isRight shouldEqual true
    val resultFile = result.right.get.head
    resultFile.name.endsWith("postman_collection.json") shouldEqual true
    val postmanCollectionJson = Json.parse(resultFile.contents)

    jsonAssertion(postmanCollectionJson)
  }

}
