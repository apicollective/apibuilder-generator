package generator

import examples.{ExampleJson, Selection}
import generator.Heuristics.PathVariable
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.generator.CodeGenerator
import io.flow.postman.generator.attributes.v0.models._
import io.flow.postman.generator.attributes.v0.models.json.jsonReadsPostmanGeneratorAttributesBasicAuth
import io.flow.postman.v0.{models => postman}
import io.flow.postman.v0.models.json._
import models.service.ResolvedService
import play.api.libs.json.Json
import Utils._
import io.flow.postman.v0.models.{Folder, Item}
import org.scalactic.TripleEquals._

object PostmanCollectionGenerator extends CodeGenerator {

  import scala.languageFeature.implicitConversions._
  import models.attributes.PostmanAttributes._

  object Constants {
    val BaseUrl = "BASE_URL"
    val EntitiesSetup = "Entities Setup"
    val EntitiesCleanup = "Entities Cleanup"
  }

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

    val examplesProvider: ExampleJson = ExampleJson(service, Selection.All)

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

    val (entitiesSetupFolderOpt, entitiesCleanupFolderOpt) =
      prepareDependantEntitiesSetupAndCleanup(resolvedService, serviceSpecificHeaders, examplesProvider)

    val postmanCollectionFolders = for {
      resource <- service.resources
      heuristicPathVariableOpt <- Seq(prepareHeuristicPathVar(resource))
      postmanItems = prepareOperations(resource, heuristicPathVariableOpt, serviceSpecificHeaders, examplesProvider)
    } yield {
      postman.Folder(
        name = resource.plural,
        description = resource.description.map(d => Description(d)),
        item = postmanItems
      )
    }

    val folders: Seq[Folder] =
      (entitiesSetupFolderOpt ++:
        postmanCollectionFolders ++:
        entitiesCleanupFolderOpt).toSeq

    postman.Collection(
      info = collectionInfo,
      item = folders,
      variable = Seq(
        Utils.Variable(
          key = Constants.BaseUrl,
          value = baseUrl.getOrElse(""),
          `type` = "string"
        )
      ),
      auth = basicAuthOpt,
      event = Seq.empty
    )
  }

  def prepareHeuristicPathVar(resource: Resource): Option[PathVariable] = {
    Heuristics
      .idFieldHeuristic(resource)
      .map {
        pathVar => PathVariable(pathVar, s"${resource.plural}-$pathVar")
      }
  }

  def prepareOperations(
    resource: Resource,
    pathVariableOpt: Option[PathVariable],
    serviceSpecificHeaders: Seq[postman.Header],
    examplesProvider: ExampleJson) = {

    resource
      .operations
      .map(PostmanItemBuilder.build(_, serviceSpecificHeaders, examplesProvider, pathVariableOpt))
      .map(addItemTests)
  }

  /**
    * Prepares dependant entities setup phase.
    *
    * @param resolvedService Service with all imports at hand.
    * @param serviceSpecificHeaders Service specific headers.
    * @param examplesProvider Examples generator.
    * @return Tuple of options of dependant entities setup and cleanup folders for Postman.
    */
  def prepareDependantEntitiesSetupAndCleanup(
    resolvedService          : ResolvedService,
    serviceSpecificHeaders   : Seq[postman.Header],
    examplesProvider         : ExampleJson
  ): (Option[Folder], Option[Folder]) = {
    val objReferenceAttrToOperationTuples = DependantOperationResolver.resolve(resolvedService)

    val setupItemToCleanupItemOpts = objReferenceAttrToOperationTuples.map {
      case (objRefAttr, operation) =>

        val setupItem = PostmanItemBuilder.build(operation.referencedOperation, serviceSpecificHeaders, examplesProvider, None)
        val setupItemWithTests = addItemTests(setupItem)
        val setupWithTestsAndVar = addDependencyItemVarSetting(objRefAttr, setupItemWithTests, None)

        val deleteItemOpt = operation.deleteOperationOpt
            .map(prepareDeleteOpWithFilledParam(objRefAttr, _, serviceSpecificHeaders, examplesProvider))

        setupWithTestsAndVar -> deleteItemOpt
    }

    val setupSteps = setupItemToCleanupItemOpts.map(_._1)
    val cleanupSteps = setupItemToCleanupItemOpts.flatMap(_._2)

    val setupFolderOpt = wrapInFolderIfNotEmpty(setupSteps, Constants.EntitiesSetup)
    val cleanupFolderOpt = wrapInFolderIfNotEmpty(cleanupSteps, Constants.EntitiesCleanup)

    setupFolderOpt -> cleanupFolderOpt
  }

  private def addItemTests(item: postman.Item): postman.Item = {

    val method = item.request.method.getOrElse("").toString.toUpperCase
    if (Seq("GET", "PUT", "POST").contains(method)) {
      val test = PredefinedCollectionItems.testEventResponseStatusOk(
        f"$method requests should return 2xx"
      )
      item.copy(event = Option(item.event.toSeq.flatten :+ test))
    } else {
      item
    }
  }

  private def addDependencyItemVarSetting(objRefAttr: ExtendedObjectReference, item: postman.Item, varNameOpt: Option[String]): postman.Item = {

    val varName = objRefAttr.postmanVariableName.name

    val scriptExecFragment = Seq(
      """var jsonData = JSON.parse(responseBody);""",
      s"""var id = jsonData["${objRefAttr.identifierField}"];""",
      s"""if (id != null) pm.environment.set("$varName", id);"""
    )
    item.event
      .getOrElse(Seq.empty)
      .find(_.listen === postman.EventType.Test) match {
      case Some(testEvent) =>
        val updatedScript = testEvent.script.map(_.copy(
          exec = testEvent.script.map(_.exec ++ scriptExecFragment).getOrElse(Seq.empty)
        ))
        val updatedTestEvent = testEvent.copy(script = updatedScript)
        val updatedEvents =
          item.event.getOrElse(Seq.empty)
            .filterNot(_.listen === postman.EventType.Test) :+ updatedTestEvent
        item.copy(event = Some(updatedEvents))
      case None =>
        val eventToAdd = postman.Event(
          listen = postman.EventType.Test,
          script = Some(postman.Script(
            exec = scriptExecFragment
          ))
        )
        item.copy(event = Some(Seq(eventToAdd)))
    }
  }

  private def prepareDeleteOpWithFilledParam(
    objRefAttr: ExtendedObjectReference,
    deleteOperation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    examplesProvider: ExampleJson): Item = {

    val (pathParameters, otherParams) = deleteOperation.parameters.partition(_.location === ParameterLocation.Path)
    val filledPathParameters = pathParameters match {
      case params if pathParameters.nonEmpty =>
        val updatedLast = params.lastOption.get.copy(example = Some(objRefAttr.postmanVariableName.reference))
        params.init :+ updatedLast
      case _ =>
        val rawPathParamOpt =
          deleteOperation
            .path
            .split('/')
            .reverse
            .find(_.startsWith(":"))
            .map(_.stripPrefix(":"))

        rawPathParamOpt.map { rawPathParam =>
          Parameter(
            name = rawPathParam,
            `type` = "string",
            location = ParameterLocation.Path,
            example = Some(objRefAttr.postmanVariableName.reference),
            required = true
          )
        }.toSeq
    }
    val filledDeleteOp = deleteOperation.copy(parameters = filledPathParameters ++ otherParams)

    val postmanItem = PostmanItemBuilder.build(filledDeleteOp, serviceSpecificHeaders, examplesProvider, None)
    addItemTests(postmanItem)
  }

  private def wrapInFolderIfNotEmpty(items: Seq[Item], folderName: String): Option[Folder] = {
    if (items.nonEmpty) {
      Some(
        postman.Folder(
          name = folderName,
          item = items
        ))
    } else None
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
