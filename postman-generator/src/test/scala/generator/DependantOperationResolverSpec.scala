package generator

import io.apibuilder.spec.v0.models._
import io.flow.postman.generator.attributes.v0.models.{AttributeName, ObjectReference}
import io.flow.postman.generator.attributes.v0.models.json._
import models.attributes.PostmanAttributes._
import models.operation.DependantOperations
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsObject, Json}


class DependantOperationResolverSpec extends WordSpec with Matchers {

  import models.TestHelper.{referenceApiService, generatorApiServiceWithUnionWithoutDescriminator}

  "DependantOperationResolver" should {

    "resolve simple dependencies" in new TestCtxWithImportedService {
      val resolvedService = ServiceImportResolver.resolveService(testMainService, Seq(referenceApiService))
      val result = DependantOperationResolver.resolve(resolvedService)

      result should === (List(objectRef1AttrValue.toExtended -> dependency1Target))
    }

    "resolve simple dependency with a cleanup step" in new TestCtxWithImportedService {
      val resolvedService = ServiceImportResolver.resolveService(testMainService, Seq(updatedReferenceApiService))
      val result = DependantOperationResolver.resolve(resolvedService)

      val cleanupOp = getTargetOperation(updatedReferenceApiService, objectRef1AttrValue).deleteOperationOpt.get

      result.nonEmpty shouldEqual true
      val dependantOperations = result.head._2
      dependantOperations.deleteOperationOpt.isDefined shouldEqual true
      dependantOperations.deleteOperationOpt.get shouldEqual cleanupOp
    }

    "resolve second level dependencies (dependency has its own dependency) in the right order" in new TestCtxWithImportedService {
      val objRef2AttrValue = ObjectReference(
        relatedServiceNamespace = referenceApiService.namespace,
        resourceType = "group",
        operationMethod = Method("GET"),
        identifierField = "members[0].age_group"
      )
      val dependency2Target = getTargetOperation(referenceApiService, objRef2AttrValue)
      val testReferenceApiService = addAttributeToModelField(referenceApiService, "member", "role", objRef2AttrValue)

      val resolvedService = ServiceImportResolver.resolveService(testMainService, Seq(testReferenceApiService))
      val result = DependantOperationResolver.resolve(resolvedService)


      val expected: Seq[(ExtendedObjectReference, DependantOperations)] = Seq(
        objRef2AttrValue -> dependency2Target,
        objectRef1AttrValue -> dependency1Target
      ).toExtended

      result should contain allElementsOf expected
    }

    "resolve nested dependencies that span across 3 different services (main<-import1<-import2)" in new TestCtxWithTwoImportedServices {
      val objRefToThirdServiceAttrValue = ObjectReference(
        relatedServiceNamespace = generatorApiServiceWithUnionWithoutDescriminator.namespace,
        resourceType = "user",
        operationMethod = Method("POST"),
        identifierField = "guid"
      )
      val dependencyToThirdServiceTarget = {
        val dependantOperations = getTargetOperation(generatorApiServiceWithUnionWithoutDescriminator, objRefToThirdServiceAttrValue)
        val referencedOperation = dependantOperations.referencedOperation
        val updatedBody = referencedOperation.body.get.copy(`type` = objRefToThirdServiceAttrValue.relatedServiceNamespace + ".unions." + referencedOperation.body.get.`type`)
        val updatedReferencedOperation = referencedOperation.copy(body = Some(updatedBody))
        dependantOperations.copy(referencedOperation = updatedReferencedOperation)
      }
      val testReferenceApiService = addAttributeToModelField(referenceApiService, "member", "role", objRefToThirdServiceAttrValue)

      val resolvedService = ServiceImportResolver.resolveService(testMainServiceWithTwoImports, Seq(testReferenceApiService, generatorApiServiceWithUnionWithoutDescriminator))
      val result = DependantOperationResolver.resolve(resolvedService)


      result shouldEqual Seq(
        objRefToThirdServiceAttrValue -> dependencyToThirdServiceTarget,
        objectRef1AttrValue -> dependency1Target
      ).toExtended
    }

    "resolve a dependency that exists in a nested model" in new TestCtxWithImportedService {

      val childComplexType = "child-complex-type"

      override val testMainService = trivialServiceWithImport.copy(
        models = Seq(
          Model( // child complex type, that contains a dependency
            name = childComplexType,
            plural = childComplexType + "s",
            fields = Seq(
              Field(
                name = "value",
                `type` = "string",
                example = Some("something"),
                required = true,
                attributes = modelWithDependency.fields.head.attributes
              )
            )
          ),
          Model( // artificial complex type on top of another complex type - resource references this model
            name = modelWithDependency.name,
            plural = modelWithDependency.plural,
            fields = Seq(
              Field(
                name = "complex",
                `type` = childComplexType,
                required = true
              )
            )
          )
        )
      )

      val resolvedService = ServiceImportResolver.resolveService(testMainService, Seq(referenceApiService))
      val result = DependantOperationResolver.resolve(resolvedService)

      result shouldEqual Seq(
        objectRef1AttrValue -> dependency1Target
      ).toExtended
    }

  }

  implicit class SeqObjectRefOperationExtend(seq: Seq[(ObjectReference, DependantOperations)]) {
    def toExtended: Seq[(ExtendedObjectReference, DependantOperations)] = {
      seq.map {
        case (ref, op) => (ref.toExtended, op)
      }
    }
  }

  private def getTargetOperation(targetService: Service, objRefAttrValue: ObjectReference): DependantOperations = {
    val targetResource =
      targetService
        .resources.find(_.`type` === objRefAttrValue.resourceType).get

    val referencedOp = targetResource
      .operations.find(_.method === objRefAttrValue.operationMethod).get
    val deleteOpOption = objRefAttrValue.deleteOperationPath.flatMap { deleteOpPath =>
      targetResource
        .operations.find(_.path === deleteOpPath)
    }

    DependantOperations(referencedOp, deleteOpOption)
  }

  private def addAttributeToModelField(service: Service, modelName: String, fieldName: String, attributeValue: ObjectReference): Service = {
    val oldModel = service
      .models.find(_.name == modelName).get

    val updatedRoleField = oldModel
      .fields.find(_.name == fieldName).get
      .copy(
        attributes = Seq(
          Attribute(
            AttributeName.ObjectReference.toString,
            Json.toJson(attributeValue).as[JsObject]
          )
        )
      )

    val otherFields = oldModel.fields.filterNot(_.name == fieldName)
    val updatedMemberModel = oldModel.copy(
      fields = otherFields :+ updatedRoleField
    )

    val otherModels = service.models.filterNot(_.name == modelName)

    service.copy(
      models = otherModels :+ updatedMemberModel
    )
  }

  trait TestDependencies extends TestFixtures.TrivialServiceWithImportAndDependencyCtx {
    val dependency1Target = getTargetOperation(referenceApiService, objectRef1AttrValue)
  }

  trait TestCtxWithImportedService extends TestDependencies with TestFixtures.TrivialServiceWithImportCtx {
    val testMainService = trivialServiceWithImport.copy(
      models = Seq(modelWithDependency)
    )
  }

  trait TestCtxWithTwoImportedServices extends TestDependencies with TestFixtures.TrivialServiceWithTwoImportsCtx {
    val testMainServiceWithTwoImports = trivialServiceWithTwoImports.copy(
      models = Seq(modelWithDependency)
    )
  }

}
