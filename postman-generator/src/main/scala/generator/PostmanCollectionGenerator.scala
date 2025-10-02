package generator

import examples.{ExampleJson, RandomStringGenerator, UuidRandomStringGenerator, Selection}
import generator.Heuristics.PathVariable
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.*
import lib.generator.CodeGenerator
import io.postman.generator.attributes.v0.models.*
import io.postman.generator.attributes.v0.models.json.jsonReadsPostmanGeneratorAttributesBasicAuth
import io.apibuilder.postman.collection.v21.v0.{models => postman}
import io.apibuilder.postman.collection.v21.v0.models.json.*
import models.service.ResolvedService
import play.api.libs.json.Json
import Utils.*
import io.apibuilder.postman.collection.v21.v0.models.{Folder, Item}
import lib.Datatype.Primitive

class PostmanCollectionGenerator(randomStringGenerator: RandomStringGenerator) extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    form.importedServices match {
      case None if form.service.imports.nonEmpty =>
        Left(Seq("Service imports need to be resolved before generating Postman Collection. However, InvocationForm.importedServices is empty"))
      case None => invokeGenerator(form.service, Seq.empty)
      case Some(importedServices) => invokeGenerator(form.service, importedServices)
    }
  }

  private def invokeGenerator(service: Service, importedServices: Seq[Service]): Either[Seq[String], Seq[File]] = {

    val resolvedService: ResolvedService = ServiceImportResolver.resolveService(service, importedServices)
    val postmanCollectionJson = generatePostmanCollection(resolvedService)
    val savedFile = writePostmanCollectionToFile(service, postmanCollectionJson)

    Right(Seq(savedFile))
  }

  private def generatePostmanCollection(resolvedService: ResolvedService): postman.Collection = {

    val service: Service = resolvedService.service

    val collectionInfo = postman.Info(
      name = service.name,
      postmanId = None,
      description = Some(postman.Description(content = service.description, `type` = None)),
      version = Some(service.version),
      schema = "https://schema.getpostman.com/json/collection/v2.1.0/collection.json"
    )

    val baseUrl = service.baseUrl

    val serviceSpecificHeaders = service.headers.map { header =>
      postman.Header(
        key = header.name,
        value = header.default.getOrElse(""),
        description =
          header.description
            .orElse(Some(s"Type: ${header.`type`}  | Required: ${header.required}"))
            .map(Description(_)),
        disabled = None
      )
    }

    val examplesProvider: ExampleJson = ExampleJson(service, Selection.All, randomStringGenerator)

    val basicAuthOpt = service.attributes
      .find(_.name.equalsIgnoreCase(AttributeName.PostmanBasicAuth.toString))
      .flatMap(_.value.asOpt[BasicAuth])
      .map { basicAuth =>
        postman.Auth(
          `type` = postman.AuthEnum.Basic,
          basic = Some(List(
            postman.BasicAuth(key = "username", value = basicAuth.username),
            postman.BasicAuth(key = "password", value = basicAuth.password)
          ))
        )
      }

    val objReferenceAttrToOperationTuples = DependantOperationResolver.resolve(resolvedService)

    val (entitiesSetupFolderOpt, entitiesCleanupFolderOpt) =
      SetupCleanupFolderBuilder.prepareDependantEntitiesSetupAndCleanup(objReferenceAttrToOperationTuples, serviceSpecificHeaders, examplesProvider)

    val postmanCollectionFolders = for {
      resource <- service.resources
      heuristicPathVariableOpt = None
      postmanItems = prepareOperations(resource, heuristicPathVariableOpt, serviceSpecificHeaders, examplesProvider)
    } yield {
      postman.Folder(
        name = resource.plural,
        description = resource.description.map(d => Description(d)),
        item = postmanItems
      )
    }

    val folders: Seq[Folder] =
      (entitiesSetupFolderOpt ++
        postmanCollectionFolders ++
        entitiesCleanupFolderOpt).toSeq

    postman.Collection(
      info = collectionInfo,
      item = folders,
      variable = Seq(
        Utils.Variable(
          key = PostmanGeneratorConstants.BaseUrl,
          value = baseUrl.getOrElse(""),
          `type` = Primitive.String.name
        )
      ),
      auth = basicAuthOpt,
      event = Seq.empty
    )
  }


  /*
  Disabled for now
  
  private def prepareHeuristicPathVar(resource: Resource): Option[PathVariable] = {
    Heuristics
      .idFieldHeuristic(resource)
      .map {
        pathVar => PathVariable(pathVar, s"${resource.plural}-$pathVar")
      }
  }*/

  private def prepareOperations(
    resource: Resource,
    pathVariableOpt: Option[PathVariable],
    serviceSpecificHeaders: Seq[postman.Header],
    examplesProvider: ExampleJson): Seq[Item] = {

    resource
      .operations
      .map(PostmanItemBuilder.build(_, serviceSpecificHeaders, examplesProvider, pathVariableOpt))
      .map(PredefinedCollectionItems.addItemTests)
  }

  private def writePostmanCollectionToFile(service: Service, postmanCollection: postman.Collection): File = {

    val postmanCollectionJson = Json.toJson(postmanCollection)
    val jsonPrettyPrint = Json.prettyPrint(postmanCollectionJson) + "\n"

    ServiceFileNames.toFile(
      namespace = service.namespace,
      organizationKey = service.organization.key,
      applicationKey = service.application.key,
      version = service.version,
      suffix = "PostmanCollection",
      contents = jsonPrettyPrint,
      languages = Some("json")
    )
  }
}

object PostmanCollectionGeneratorImpl extends PostmanCollectionGenerator(UuidRandomStringGenerator)
