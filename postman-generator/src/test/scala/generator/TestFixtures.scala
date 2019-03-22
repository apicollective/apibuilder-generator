package generator

import io.apibuilder.spec.v0.models._
import io.flow.postman.generator.attributes.v0.models.{AttributeName, ObjectReference}
import io.flow.postman.generator.attributes.v0.models.json.jsonWritesPostmanGeneratorAttributesObjectReference
import lib.Datatype.Primitive
import models.operation.DependantOperations
import play.api.libs.json.{JsObject, Json}
import org.scalactic.TripleEquals._

object TestFixtures {

  import models.TestHelper.{referenceApiService, generatorApiServiceWithUnionWithoutDescriminator}

  trait TrivialServiceContext {
    val trivialService = Service(
      apidoc = Apidoc("0.1"),
      name = "trivial",
      organization = Organization("test-org"),
      application = Application("test-app"),
      namespace = "io.trivial",
      version = "0.1",
      info = Info(license = None, contact = None),
      models = Seq(
        Model(
          name = "complex-string",
          plural = "complex-strings",
          fields = Seq(
            Field(
              name = "value",
              `type` = Primitive.String.name,
              example = Some("something"),
              required = true
            )
          ))
      ),
      resources = Seq(
        Resource(
          `type` = "complex-string",
          plural = "complex-strings",
          operations = Seq(
            Operation(
              method = Method.Post,
              path = "/complex-strings/new",
              body = Some(
                Body(`type` = "complex-string")
              )
            )
          )
        )
      ),
      baseUrl = Some("https://some.service.com")
    )

    def getTargetOperation(targetService: Service, objRefAttrValue: ObjectReference): DependantOperations = {
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

  }

  trait TrivialServiceWithImportCtx extends TrivialServiceContext {
    val importedEnum = referenceApiService.enums.find(_.name == "age_group")
      .getOrElse(throw new NoSuchElementException("age_group enum is expected in example reference-service.json"))
    val importedEnumPath = s"${referenceApiService.namespace}.enums.age_group"

    val modelWithImportedEnum = Model(
      name = "age",
      plural = "ages",
      fields = Seq(
        Field(
          name = "group",
          `type` = importedEnumPath,
          required = true
        )
      )
    )

    val resourceWithImportedEnum = Resource(
      `type` = "age",
      plural = "ages",
      operations = Seq(
        Operation(
          method = Method.Get,
          path = "/ages/first",
          responses = Seq(
            Response(
              code = ResponseCodeInt(200),
              `type` = "age"
            )
          )
        )
      )
    )

    val trivialServiceWithImport = trivialService.copy(
      imports = Seq(
        Import(
          uri = "some-uri",
          namespace = referenceApiService.name,
          organization = referenceApiService.organization,
          application = referenceApiService.application,
          version = referenceApiService.version,
          enums = Seq("age_group")
        )
      ),
      models = trivialService.models :+ modelWithImportedEnum,
      resources = trivialService.resources :+ resourceWithImportedEnum
    )

  }

  trait TrivialServiceWithImportAndDependencyCtx extends TrivialServiceWithImportCtx {
    private val membersResource = referenceApiService.resources(2)
    private val postSingleMemberOp = membersResource.operations(0).copy(
      parameters = Seq.empty,
      body = Some(Body("member"))
    )
    private val updatedOperationsList = membersResource.operations.updated(0, postSingleMemberOp)
    private val extraDeleteOpPath = "/members/:id/delete"
    private val extraDeleteOp = Operation(
      method = Method.Delete,
      path = extraDeleteOpPath,
      parameters = Seq(
        Parameter(
          name = "id",
          `type` = Primitive.String.name,
          location = ParameterLocation.Path,
          required = true
        )
      ),
      body = None
    )
    private val updatedMembersResource = membersResource.copy(operations = updatedOperationsList :+ extraDeleteOp)
    val updatedReferenceApiService = referenceApiService.copy(
      resources = referenceApiService.resources.updated(2, updatedMembersResource)
    )

    val objectRef1AttrValue = ObjectReference(
      relatedServiceNamespace = referenceApiService.namespace,
      resourceType = "member",
      operationMethod = Method("POST"),
      identifierField = "guid",
      operationPath = "/members",
      deleteOperationPath = Some(extraDeleteOpPath)
    )

    val objectRef1Attribute = Attribute(
      name = AttributeName.ObjectReference.toString,
      value = Json.toJson(objectRef1AttrValue).as[JsObject]
    )

    val modelWithDependency = Model(
      name = "complex-string",
      plural = "complex-strings",
      fields = Seq(
        Field(
          name = "value",
          `type` = Primitive.String.name,
          example = Some("something"),
          required = true,
          attributes = Seq(objectRef1Attribute)
        )
      )
    )

    val trivialServiceWithImportAndDependency = trivialServiceWithImport.copy(
      models = trivialServiceWithImport.models.updated(0, modelWithDependency)
    )
  }
  
  trait TrivialServiceWithUnionTypesImportCtx extends TrivialServiceContext {

    val unionType = "foobar"
    val importedUnion = generatorApiServiceWithUnionWithoutDescriminator.unions.find(_.name == unionType)
      .getOrElse(throw new NoSuchElementException(s"$unionType is expected in example apidoc-example-union-types.json"))
    val importedUnionPath = s"${generatorApiServiceWithUnionWithoutDescriminator.namespace}.unions.$unionType"

    val modelWithImportedUnion = Model(
      name = "union",
      plural = "unions",
      fields = Seq(
        Field(
          name = "my-union",
          `type` = importedUnionPath,
          required = true
        )
      )
    )

    val resourceWithImportedUnion = Resource(
      `type` = "union",
      plural = "unions",
      operations = Seq(
        Operation(
          method = Method.Get,
          path = "/unions/my",
          responses = Seq(
            Response(
              code = ResponseCodeInt(200),
              `type` = "union"
            )
          )
        )
      )
    )

    val trivialServiceWithUnionTypesImport = trivialService.copy(
      imports = Seq(
        Import(
          uri = "some-uri-2",
          namespace = generatorApiServiceWithUnionWithoutDescriminator.name,
          organization = generatorApiServiceWithUnionWithoutDescriminator.organization,
          application = generatorApiServiceWithUnionWithoutDescriminator.application,
          version = generatorApiServiceWithUnionWithoutDescriminator.version,
          unions = Seq(unionType)
        )
      ),
      models = trivialService.models :+ modelWithImportedUnion,
      resources = trivialService.resources :+ resourceWithImportedUnion
    )
  }

  trait TrivialServiceWithTwoImportsCtx extends TrivialServiceWithImportCtx with TrivialServiceWithUnionTypesImportCtx {
    val trivialServiceWithTwoImports = trivialService.copy(
      imports = trivialServiceWithImport.imports ++ trivialServiceWithUnionTypesImport.imports,
      models = trivialService.models :+ modelWithImportedEnum :+ modelWithImportedUnion,
      resources = trivialService.resources :+ resourceWithImportedEnum :+ resourceWithImportedUnion
    )
  }

}
