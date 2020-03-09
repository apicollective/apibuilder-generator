package generator

import examples.{ExampleJson, MurmurRandomStringGenerator, Selection}
import generator.TestFixtures.TrivialServiceWithImportAndDependencyCtx
import io.apibuilder.postman.collection.v21.v0.models.{EventType, Folder, Header}
import models.attributes.PostmanAttributes.ExtendedObjectReference
import models.operation.DependantOperations
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SetupCleanupFolderBuilderSpec extends AnyWordSpec with Matchers {

  import models.attributes.PostmanAttributes.ObjectReferenceExtend

  "SetupCleanupFolderBuilder" should {

    "fill query params with the values from object-reference attribute in Setup phase" in new TestContext {
      val queryParamMap = Map("param" -> "value")
      val objRefWithQueryParams = objectRef1AttrValue.copy(
        queryParams = Some(queryParamMap)
      )
      val targetOp = getTargetOperation(importedService, objRefWithQueryParams)
      override lazy val objReferenceAttrToOperationTuples: Seq[(ExtendedObjectReference, DependantOperations)] =
        Seq(objRefWithQueryParams.toExtended -> targetOp)

      setupFolder.item.foreach { i =>
        val queryParamsOpt = i.request.url.get.query
        queryParamsOpt.exists(_.nonEmpty) shouldEqual true
        queryParamsOpt.get.foreach { queryParam =>
          queryParam.disabled shouldEqual Some(false)
          queryParam.value.isDefined shouldEqual true
          queryParam.value shouldEqual queryParamMap.get(queryParam.key.get)
        }
      }
      setupFolder.item.nonEmpty shouldEqual true
    }

    "fill variable setting script in 'Test' section of Setup phase items" in new TestContext {
      setupFolder.item.foreach { item =>
        item.event.isDefined shouldEqual true
        val testEvents = item.event.get.filter(_.listen == EventType.Test)
        testEvents.size shouldEqual 1
        val scriptLines = testEvents.head.script.get.exec.mkString("\n")
        scriptLines should include("pm.environment.set")
        scriptLines should include(objectRef1AttrValue.toExtended.postmanVariableName.name)
      }
      setupFolder.item.nonEmpty shouldEqual true
    }

    "fill path param with variable reference in Cleanup phase" in new TestContext {
      cleanupFolder.item.nonEmpty shouldEqual true
      cleanupFolder.item.head.request.url.get.variable.exists(_.nonEmpty) shouldEqual true
      val deleteOpPathParam = cleanupFolder.item.head.request.url.get.variable.get.head
      deleteOpPathParam.value shouldEqual Some(objectRef1AttrValue.toExtended.postmanVariableName.reference)
    }

    "add service-specific headers to the Setup and Cleanup phases" in new TestContext {
      override lazy val serviceSpecificHeaders: Seq[Header] = Seq(Header("specific", "value", disabled = Some(false)))

      def checkFolder(folder: Folder): Assertion = {
        folder.item.foreach { item =>
          item.request.header.exists(_.nonEmpty) shouldEqual true
          item.request.header.get should contain allElementsOf serviceSpecificHeaders
        }
        folder.item.nonEmpty shouldEqual true
      }

      checkFolder(setupFolder)
      checkFolder(cleanupFolder)
    }

  }

  trait TestContext extends TrivialServiceWithImportAndDependencyCtx {
    val importedService = updatedReferenceApiService
    val testMainService = ServiceImportResolver.resolveService(trivialServiceWithImportAndDependency, Seq(importedService)).service

    val defaultTargetOp = getTargetOperation(importedService, objectRef1AttrValue)

    lazy val objReferenceAttrToOperationTuples: Seq[(ExtendedObjectReference, DependantOperations)] =
      Seq(objectRef1AttrValue.toExtended -> defaultTargetOp)
    lazy val serviceSpecificHeaders: Seq[Header] = Seq.empty
    lazy val exampleProvider: ExampleJson = ExampleJson(testMainService, Selection.All, MurmurRandomStringGenerator)

    lazy val (setupFolderOpt, cleanupFolderOpt) =
      SetupCleanupFolderBuilder.prepareDependantEntitiesSetupAndCleanup(objReferenceAttrToOperationTuples, serviceSpecificHeaders, exampleProvider)

    def setupFolder: Folder = setupFolderOpt getOrElse fail("Setup folder has not been created in this test case!")
    def cleanupFolder: Folder = cleanupFolderOpt getOrElse fail("Cleanup folder has not been created in this test case!")
  }

}
