package generator

import examples.{ExampleJson, Selection}
import generator.Heuristics.PathVariable
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.generator.CodeGenerator
import models.ObjectReferenceAttribute.ObjectReferenceAttrValue
import io.flow.postman.collection.v210.v0.{models=>postman}
import io.flow.postman.collection.v210.v0.models.json._
import models.service.{ResolvedService, ServiceImportResolver}
import play.api.libs.json.Json
import Utils._

object PostmanCollectionGenerator extends CodeGenerator {

  object Variables {
    val FlowToken = "FLOW_TOKEN"
    val Organization = "ORGANIZATION"
    val BaseUrl = "BASE_URL"
    val BaseUrlValue = "https://api.flow.io"
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

    val postmanCollectionFileName = s"${service.name}-postman-collection.json"
    val savedFile = writePostmanCollectionToFile(service, postmanCollectionFileName, postmanCollectionJson)

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

    val baseUrl = service.baseUrl.getOrElse(Variables.BaseUrlValue)

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

    val postmanCollectionFolders = service.resources.map { resource =>

      val pathVariableOpt =
        Heuristics
          .idFieldHeuristic(resource)
          .map {
            pathVar => PathVariable(pathVar, s"${resource.plural}-$pathVar")
          }

      val postmanItems = resource
        .operations
        .map(PostmanItemBuilder.build(baseUrl, _, serviceSpecificHeaders, examplesProvider, pathVariableOpt))
        .map(addItemTests(_, pathVariableOpt))

      postman.Folder(
        name = resource.plural,
        description = resource.description.map(d => Description(d)),
        item = postmanItems
      )
    }

    val cleanupFolder: postman.Folder = PredefinedCollectionItems.prepareCleanupFolder()
    val setupFolder: postman.Folder = PredefinedCollectionItems.prepareSetupFolder()

    val objReferenceAttrToOperationTuples = DependantOperationResolver.resolve(resolvedService)
    val requiredEntitiesSetupSteps = objReferenceAttrToOperationTuples.map {
      case (objRefAttr, operation) =>
        val postmanItem = PostmanItemBuilder.build(baseUrl, operation, serviceSpecificHeaders, examplesProvider, None)
        val postmanItemWithTests = addItemTests(postmanItem, None)

        addDependencyItemVarSetting(objRefAttr, postmanItemWithTests)
    }

    val setupFolderWithDependantEntities = setupFolder.copy(
      item = setupFolder.item ++ requiredEntitiesSetupSteps
    )

    postman.Collection(
      info = collectionInfo,
      item = postmanCollectionFolders, // TODO: remove after fixing hardcodes .:+(cleanupFolder).+:(setupFolderWithDependantEntities),
      variable = Seq(
        Variable(
          key = Variables.BaseUrl,
          value = Variables.BaseUrlValue,
          `type` = "string"
        )
      ),
      auth =
        Some(postman.Auth(
        `type` = postman.AuthEnum.Basic,
        basic = Some(List(
          postman.BasicAuth(key = "username", value = s"{{${Variables.FlowToken}}}"),
          postman.BasicAuth(key = "password", value = "")
        ))
      )),
      event = Seq.empty
    )
  }

  private def addItemTests(item: postman.Item, pathVariableOpt: Option[PathVariable]): postman.Item = {

    def isParametrizedOnlyWithOrg(item: postman.Item): Boolean = {
      import item.request

      request.url.map { url =>
        url.variable.size == 1 &&
          url.variable.headOption.forall(_.forall(_.name.getOrElse("") == s"{{${Variables.Organization}}}")) &&
          url.query.getOrElse(List.empty)
            .filterNot(_.disabled.getOrElse(false))
            .forall(_.value.isDefined)
      }.getOrElse(false)
    }

    def methodEquals(item: postman.Item, method: String): Boolean =
      item.request.method.getOrElse("").toString.equalsIgnoreCase(method)

    item match {
      case simpleGet if methodEquals(simpleGet, "GET") && isParametrizedOnlyWithOrg(simpleGet) =>
        val test = PredefinedCollectionItems.testEventResponseStatusOk(
          "GET requests parametrized only by Organization should return 2xx"
        )
        item.copy(event = item.event.map(_ :+ test))
      case simplePost if methodEquals(simplePost, "POST") && isParametrizedOnlyWithOrg(simplePost) =>
        val test = PredefinedCollectionItems.testPostStatusOk(
          "POST requests parametrized only by Organization should return 2xx",
          pathVariableOpt
        )
        item.copy(event = item.event.map(_ :+ test))
      case other =>
        other
    }
  }

  private def addDependencyItemVarSetting(objRefAttr: ObjectReferenceAttrValue, item: postman.Item): postman.Item = {

    val scriptExecFragment = Seq(
      """var jsonData = JSON.parse(responseBody);""",
      s"""var id = jsonData["${objRefAttr.identifierField}"];""",
      s"""if (id != null) pm.environment.set("${objRefAttr.toPostmanVariableName}", id);"""
    )

    item.
      event
      .getOrElse(Seq.empty)
      .find(_.listen == postman.EventType.Test) match {
          case Some(testEvent) =>
            val updatedScript = testEvent.script.map(_.copy(
              exec = testEvent.script.map(_.exec ++ scriptExecFragment).getOrElse(Seq.empty)
            ))
            val updatedTestEvent = testEvent.copy(script = updatedScript)
            val updatedEvents =
              item.event.getOrElse(Seq.empty)
                .filterNot(_.listen == postman.EventType.Test) :+ updatedTestEvent
            item.copy(event = Some(updatedEvents))
          case None =>
            val eventToAdd = postman.Event(
              listen = postman.EventType.Test,
              script = Some(postman.Script(
                exec = scriptExecFragment
              ))
            )
            item.copy(event = item.event.map(_ :+ eventToAdd))
        }

  }

  private def writePostmanCollectionToFile(service: Service, fileName: String, postmanCollection: postman.Collection): File = {

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
