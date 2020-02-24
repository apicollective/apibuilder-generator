package generator

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper._
import org.scalatest.Assertion
import play.api.libs.json._
import testUtils.TestPostmanCollectionGenerator

import scala.io.Source
import org.scalatest.wordspec.AnyWordSpec

class FileInputOutputGeneratorTests extends AnyWordSpec {

  "Postman Generator" should {
    "successfully generate a postman collection from test-service-1.json" in {
      generateCollectionAndVerify("/apibuilder/test-service-1.json", "/postman/expected-1.json")
    }

    "successfully generate a postman collection from test-service-operation-deps.json" in {
      generateCollectionAndVerify("/apibuilder/test-service-operation-deps.json", "/postman/expected-operation-deps.json")
    }

  }

  private def generateCollectionAndVerify(servicePath: String, expectedCollectionPath: String): Assertion = {
    val resourceUrl = getClass.getResource(expectedCollectionPath)
    val expectedJson = Json.parse(
      Source.fromURL(resourceUrl).mkString
    ).as[JsValue]

    val inputFile = Source.fromURL(getClass.getResource(servicePath)).mkString
    val parsedService = service(inputFile)

    val invocationForm = InvocationForm(parsedService, importedServices = None)
    val result = TestPostmanCollectionGenerator.invoke(invocationForm)

    val files = result.getOrElse(fail("Generator invoke failure"))
    val str = files.head.contents
    val outputJsonCollection = Json.parse(str)

    outputJsonCollection shouldEqual expectedJson
  }
}