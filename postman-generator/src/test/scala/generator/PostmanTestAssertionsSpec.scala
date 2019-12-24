package generator

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.postman.collection.v21.v0.models._
import io.apibuilder.postman.collection.v21.v0.models.json.jsonReadsApiBuilderPostmanCollectionV21Collection
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json
import testUtils.TestPostmanCollectionGenerator

class PostmanTestAssertionsSpec extends WordSpec with Matchers {

  import models.TestHelper.referenceApiService

  "Postman JS test assertions" should {

    "be added to GET, PUT, POST requests as simple 2xx status code check" in new TestContext {
      val invocationForm = InvocationForm(referenceApiService, importedServices = None)
      val result = TestPostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollection(result) { collection =>
        val folders = collection.item.collect {
          case folder: Folder => folder
        }
        val items = folders.flatMap(_.item)
        val itemsWithTests = items.filter { item =>
          val req = item.request
          req.method.isDefined && testedMethods.contains(req.method.get)
        }

        itemsWithTests.foreach { item =>
          item.event.isDefined shouldEqual true
          val testOpt = item.event.get.find(_.listen === EventType.Test)
          testOpt.isDefined shouldEqual true
          val scriptOpt = testOpt.get.script
          scriptOpt.isDefined shouldEqual true
          val isTested = scriptOpt.get.exec.exists(_.trim === "pm.response.to.be.success;")
          assert(isTested, "This request should contain a JavaScript Postman test statement ")
        }
      }
    }

    "not be added to the requests with the remaining methods" in new TestContext {
      val invocationForm = InvocationForm(referenceApiService, importedServices = None)
      val result = TestPostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollection(result) { collection =>
        val folders = collection.item.collect {
          case folder: Folder => folder
        }
        val items = folders.flatMap(_.item)
        val itemsWithoutTests = items.filter { item =>
          val req = item.request
          req.method.isDefined && !testedMethods.contains(req.method.get)
        }

        itemsWithoutTests.foreach { item =>
          item.event.isDefined shouldEqual false
        }
      }
    }

  }

  trait TestContext {

    val testedMethods = Seq(Method("GET"), Method("PUT"), Method("POST"))

    def assertResultCollection(result: Either[Seq[String], Seq[File]])(collectionAssertion: Collection => Unit): Unit = {
      result.isRight shouldEqual true
      val resultFile = result.right.get.head
      val postmanCollection = Json.parse(resultFile.contents).as[Collection]

      collectionAssertion(postmanCollection)
    }

  }



}
