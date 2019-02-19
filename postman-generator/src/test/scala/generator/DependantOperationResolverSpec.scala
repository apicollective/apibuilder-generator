package generator

import io.apibuilder.spec.v0.models._
import models.attributes.PostmanAttributes
import models.attributes.PostmanAttributes.{ObjectReferenceAttrValue, objectReferenceAttrValueFormats}
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsObject, Json}

class DependantOperationResolverSpec extends WordSpec with Matchers {

  import models.TestHelper.{referenceApiService, generatorApiServiceWithUnionWithoutDescriminator}

  "DependantOperationResolver" should {

    "resolve simple dependencies" in new TestCtxWithImportedService {
      val resolvedService = ServiceImportResolver.resolveService(testMainService, Seq(referenceApiService))
      val result = DependantOperationResolver.resolve(resolvedService)

      result shouldEqual Seq(
        objectRef1AttrValue -> dependency1Target
      )
    }

    "resolve second level dependencies (dependency has its own dependency) in the right order" in new TestCtxWithImportedService {
      val objRef2AttrValue = ObjectReferenceAttrValue(
        relatedServiceNamespace = referenceApiService.namespace,
        resourceType = "group",
        operationMethod = "GET",
        identifierField = "members[0].age_group"
      )
      val dependency2Target = getTargetOperation(referenceApiService, objRef2AttrValue)
      val testReferenceApiService = addAttributeToModelField(referenceApiService, "member", "role", objRef2AttrValue)

      val resolvedService = ServiceImportResolver.resolveService(testMainService, Seq(testReferenceApiService))
      val result = DependantOperationResolver.resolve(resolvedService)

      result shouldEqual Seq(
        objRef2AttrValue -> dependency2Target,
        objectRef1AttrValue -> dependency1Target
      )
    }

    "resolve nested dependencies that span across 3 different services (main<-import1<-import2)" in new TestCtxWithTwoImportedServices {
      val objRefToThirdServiceAttrValue = ObjectReferenceAttrValue(
        relatedServiceNamespace = generatorApiServiceWithUnionWithoutDescriminator.namespace,
        resourceType = "user",
        operationMethod = "POST",
        identifierField = "guid"
      )
      val dependencyToThirdServiceTarget = getTargetOperation(generatorApiServiceWithUnionWithoutDescriminator, objRefToThirdServiceAttrValue)
      val testReferenceApiService = addAttributeToModelField(referenceApiService, "member", "role", objRefToThirdServiceAttrValue)

      val resolvedService = ServiceImportResolver.resolveService(testMainServiceWithTwoImports, Seq(testReferenceApiService, generatorApiServiceWithUnionWithoutDescriminator))
      val result = DependantOperationResolver.resolve(resolvedService)

      result shouldEqual Seq(
        objRefToThirdServiceAttrValue -> dependencyToThirdServiceTarget,
        objectRef1AttrValue -> dependency1Target
      )
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
                attributes = trivialServiceModelWithDependency.fields.head.attributes
              )
            )
          ),
          Model( // artificial complex type on top of another complex type - resource references this model
            name = trivialServiceModelWithDependency.name,
            plural = trivialServiceModelWithDependency.plural,
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
      )
    }

  }

  private def getTargetOperation(targetService: Service, objRefAttrValue: ObjectReferenceAttrValue): Operation = {
    targetService
      .resources.find(_.`type` == objRefAttrValue.resourceType).get
      .operations.find(_.method.toString == objRefAttrValue.operationMethod).get
  }

  private def addAttributeToModelField(service: Service, modelName: String, fieldName: String, attributeValue: ObjectReferenceAttrValue): Service = {
    val oldModel = service
      .models.find(_.name == modelName).get

    val updatedRoleField = oldModel
      .fields.find(_.name == fieldName).get
      .copy(
        attributes = Seq(
          Attribute(
            PostmanAttributes.ObjectReferenceKey,
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

  trait TestDependencies {

    val objectRef1AttrValue = ObjectReferenceAttrValue(
      relatedServiceNamespace = referenceApiService.namespace,
      resourceType = "member",
      operationMethod = "POST",
      identifierField = "guid"
    )
    val dependency1Target = getTargetOperation(referenceApiService, objectRef1AttrValue)

    val trivialServiceModelWithDependency = Model(
      name = "complex-string",
      plural = "complex-strings",
      fields = Seq(
        Field(
          name = "value",
          `type` = "string",
          example = Some("something"),
          required = true,
          attributes = Seq(
            Attribute(
              PostmanAttributes.ObjectReferenceKey,
              Json.toJson(objectRef1AttrValue).as[JsObject]
            )
          )
        )
      )
    )

  }

  trait TestCtxWithImportedService extends TestDependencies with TestFixtures.TrivialServiceWithImportCtx {
    val testMainService = trivialServiceWithImport.copy(
      models = Seq(trivialServiceModelWithDependency)
    )

  }

  trait TestCtxWithTwoImportedServices extends TestDependencies with TestFixtures.TrivialServiceWithTwoImportsCtx {
    val testMainServiceWithTwoImports = trivialServiceWithTwoImports.copy(
      models = Seq(trivialServiceModelWithDependency)
    )
  }

}
