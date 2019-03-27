package generator

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper._
import org.scalatest.WordSpec
import play.api.libs.json._
import testUtils.TestPostmanCollectionGenerator

import scala.io.Source

class FileInputOutputGeneratorTests extends WordSpec {

  "Postman Generator" should {
    "successfully generate postman collection from test-service-1.json" in {

      val resourceUrl = getClass.getResource("/postman/expected-1.json")
      val expectedJson = Json.parse(
        Source.fromURL(resourceUrl).mkString
      ).as[JsValue]

      val inputFile = Source.fromURL(getClass.getResource("/apibuilder/test-service-1.json")).mkString
      val parsedService = service(inputFile)

      val invocationForm = InvocationForm(parsedService, importedServices = None)
      val result = TestPostmanCollectionGenerator.invoke(invocationForm)

      val files = result.getOrElse(fail("Generator invoke failure"))
      val str = files.head.contents
      val outputJsonCollection = Json.parse(str)

      outputJsonCollection shouldEqual expectedJson
    }
  }
}