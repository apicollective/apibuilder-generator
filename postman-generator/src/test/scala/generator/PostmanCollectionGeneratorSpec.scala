package generator

import generator.Utils.Description
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Attribute
import io.flow.postman.v0.{models => postman}
import io.flow.postman.v0.models.json.jsonReadsPostmanCollection
import io.flow.postman.generator.attributes.v0.models.AttributeName
import io.flow.postman.generator.attributes.v0.models.BasicAuth
import io.flow.postman.generator.attributes.v0.models.json.jsonWritesPostmanGeneratorAttributesBasicAuth
import org.scalatest.{Assertion, Matchers, WordSpec}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

class PostmanCollectionGeneratorSpec extends WordSpec with Matchers {

  import TestFixtures._
  import models.TestHelper._

  "PostmanCollectionGenerator" should {

    "return an error when invocation form contains a service with declared, but not provided imports" in {
      val invocationForm = InvocationForm(referenceWithImportsApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isLeft shouldEqual true
      result.left.get shouldEqual Seq("Service imports need to be resolved before generating Postman Collection. However, InvocationForm.importedServices is empty")
    }

    "return a generated Postman Collection for a trivial service" in new TrivialServiceContext {
      val invocationForm = InvocationForm(trivialService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isRight shouldEqual true

      val generatedCollectionJson = Json.parse(result.right.get.head.contents)
      generatedCollectionJson shouldEqual Json.parse(
        """
          |{
          |  "item" : [ {
          |    "item" : [ {
          |      "request" : {
          |        "method" : "POST",
          |        "header" : [ {
          |          "description" : {
          |            "content" : "Required to send JSON body"
          |          },
          |          "value" : "application/json",
          |          "key" : "Content-Type"
          |        } ],
          |        "body" : {
          |          "mode" : "raw",
          |          "raw" : "{\n  \"value\" : \"something\"\n}"
          |        },
          |        "url" : {
          |          "path" : [ "complex-strings", "new" ],
          |          "query" : [ ],
          |          "host" : [ "{{BASE_URL}}" ],
          |          "variable" : [ ],
          |          "raw" : "{{BASE_URL}}/complex-strings/new"
          |        }
          |      },
          |      "response" : [ ],
          |      "name" : "POST /complex-strings/new",
          |      "type" : "item",
          |      "event": [
          |      {
          |          "listen": "test",
          |          "script": {
          |              "exec": [
          |                  "pm.test(\"POST requests should return 2xx\", function () {",
          |                  "    pm.response.to.be.success;",
          |                  "});"
          |              ],
          |              "type": "text/javascript"
          |          }
          |      }]
          |    } ],
          |    "name" : "complex-strings",
          |    "type" : "folder"
          |  } ],
          |  "variable" : [ {
          |    "type" : "string",
          |    "value" : "https://some.service.com",
          |    "key" : "BASE_URL"
          |  } ],
          |  "event" : [ ],
          |  "info" : {
          |    "schema" : "https://schema.getpostman.com/json/collection/v2.1.0/collection.json",
          |    "name" : "trivial",
          |    "description" : { },
          |    "version" : "0.1"
          |  }
          |}
        """.stripMargin
      )
    }

    "return a generated Postman Collection for a reference service without imports" in {
      val invocationForm = InvocationForm(referenceApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collection =>
        val postmanFolders = collection.item.collect {
          case folder: postman.Folder => folder
        }
        postmanFolders.map(_.name) shouldEqual List("echoes", "groups", "members", "organizations", "users")

        val membersFolder = postmanFolders.find(_.name == "members").get
        val memberEndpoints = membersFolder.item
        val bulkCreateMemberEndpoint = memberEndpoints.find(_.name.exists(_.contains("/members/:organization/members_bulk")))
          .getOrElse(fail("reference service does not contain POST members_bulk operation"))
        val bulkCreateMemberRequest = bulkCreateMemberEndpoint.request

        val bulkCreateMemberBody = bulkCreateMemberRequest.body.get
        bulkCreateMemberBody.mode shouldEqual Some(postman.BodyMode.Raw)
        val bulkCreateMemberPayloadString = bulkCreateMemberBody.raw.get
        Try(Json.parse(bulkCreateMemberPayloadString)).isSuccess shouldEqual true

        val bulkCreateMemberRawUrl = bulkCreateMemberRequest.url.get.raw.get
        bulkCreateMemberRawUrl shouldEqual "{{BASE_URL}}/members/:organization/members_bulk"

        val bulkCreateMemberUrlVariable = bulkCreateMemberRequest.url.get.variable.get.head
        bulkCreateMemberUrlVariable.key shouldEqual Some("organization")
        bulkCreateMemberUrlVariable.value shouldEqual Some("{{ORGANIZATION}}")
        bulkCreateMemberUrlVariable.description shouldEqual Some(Description("Type: uuid  | Required: true"))
        bulkCreateMemberUrlVariable.disabled shouldEqual Some(false)

        bulkCreateMemberRequest.method shouldEqual Some(postman.Method.Post)
        val bulkCreateMemberHeader = bulkCreateMemberRequest.header.get.head
        bulkCreateMemberHeader.key shouldEqual "Content-Type"
        bulkCreateMemberHeader.value shouldEqual "application/json"
      }
    }

    "return a generated Postman Collection for a service with imports" in new TrivialServiceWithImportCtx {
      val invocationForm = InvocationForm(trivialServiceWithImport, importedServices = Some(Seq(referenceApiService)))
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collection =>
        val postmanFolders = collection.item.collect {
          case folder: postman.Folder => folder
        }
        val ageGroupsFolder = postmanFolders.find(_.name == "ages").get
        val ageGroupsEndpoints = ageGroupsFolder.item
        val getFirstAgeGroupEndpoint =
          ageGroupsEndpoints.find(_.name.exists(_.contains("/ages/first")))
          .getOrElse(fail("generated service does not contain GET /ages/first"))

        val getFirstAgeGroupEndpointResponseExample = getFirstAgeGroupEndpoint.response.get.head

        getFirstAgeGroupEndpointResponseExample.code shouldEqual Some(200)

        val responseExampleJson = Json.parse(getFirstAgeGroupEndpointResponseExample.body.get)
        val exampleGroup = (responseExampleJson \ "group").as[String]
        importedEnum.values.map(_.name) should contain(exampleGroup)
      }
    }

    "add Basic Auth definition to the Collection if the specification contains postman-basic-auth attribute" in new TrivialServiceContext {
      val basicAuthAttrValue = BasicAuth(
        username = "{{USER}}",
        password = ""
      )
      val trivialServiceWithAuth = trivialService.copy(
        attributes = Seq(
          Attribute(name = AttributeName.PostmanBasicAuth.toString, value = Json.toJson(basicAuthAttrValue).as[JsObject])
        )
      )

      val invocationForm = InvocationForm(trivialServiceWithAuth, importedServices = Some(Seq(referenceApiService)))
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collection =>
        collection.auth.isDefined shouldEqual true
        collection.auth.get.`type` shouldEqual postman.AuthEnum.Basic
        collection.auth.get.basic.get shouldEqual Seq(
          postman.BasicAuth(key = "username", value = basicAuthAttrValue.username),
          postman.BasicAuth(key = "password", value = basicAuthAttrValue.password)
        )
      }
    }

    "add Setup and Cleanup folders to the Collection if the specification contains `organization-setup` attribute" in new TrivialServiceContext {
      val trivialServiceWithFlag = trivialService.copy(
        attributes = Seq(
          Attribute("organization-setup", JsObject.empty))
      )
      val invocationForm = InvocationForm(trivialServiceWithFlag, importedServices = Some(Seq(referenceApiService)))
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collection =>
        val folderNames = collection.item.collect {
          case folder: postman.Folder => folder.name
        }
        folderNames should contain allElementsOf (Seq("Setup", "Cleanup"))
      }
    }
  }

  private def assertResultCollectionJson(result: Either[Seq[String], Seq[File]])(collectionAssertion: postman.Collection => Assertion): Assertion = {
    result.isRight shouldEqual true
    val resultFile = result.right.get.head
    resultFile.name.endsWith("postman_collection.json") shouldEqual true
    val postmanCollection = Json.parse(resultFile.contents).as[postman.Collection]

    collectionAssertion(postmanCollection)
  }

}

