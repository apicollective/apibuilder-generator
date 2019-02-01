package generator

import examples.{ExampleJson, Selection}
import generator.Heuristics.PathVariable
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.generator.CodeGenerator
import models.ObjectReferenceAttribute
import models.ObjectReferenceAttribute.ObjectReferenceAttrValue
import models.postman._
import models.postman.json._
import models.service.{ResolvedService, ServiceImportResolver}
import play.api.libs.json.Json

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
        Left(Seq("Service imports need to be resolved before generating Postman Collection. However InvocationForm.importedServices is empty"))
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

  private def generatePostmanCollection(resolvedService: ResolvedService): PostmanCollection = {

    val service: Service = resolvedService.service

    val collectionInfo = PostmanCollectionInfo(
      name = service.name,
      `_postman_id` = None,
      description = service.description
    )

    val baseUrl = service.baseUrl.getOrElse("https://api.flow.io")

    val serviceSpecificHeaders = service.headers.map { header =>
      PostmanHeader(
        key = header.name,
        value = header.default,
        description = header.description.orElse(Some(s"Type: ${header.`type`}  | Required: ${header.required}"))
      )
    }

    val examplesProvider: ExampleJson = ExampleJson(service, Selection.All)

    val postmanCollectionFolders = service.resources.map { resource =>

      val pathVariableOpt =
        Heuristics
          .idFieldHeuristic(resource)
          .map {
            pathVar => PathVariable(pathVar, s"${resource.plural}-${pathVar}")
          }

      val postmanItems = resource
        .operations
        .map(buildPostmanItem(baseUrl, _, serviceSpecificHeaders, examplesProvider, pathVariableOpt))
        .map(addItemTests(_, pathVariableOpt))

      PostmanCollectionFolder(
        name = resource.plural,
        description = resource.description,
        item = postmanItems
      )
    }

    val cleanupFolder: PostmanCollectionFolder = PredefinedCollectionItems.prepareCleanupFolder()
    val setupFolder: PostmanCollectionFolder = PredefinedCollectionItems.prepareSetupFolder()

    val objReferenceAttrToOperationTuples = generateDependantOperations(resolvedService)
    val requiredEntitiesSetupSteps = objReferenceAttrToOperationTuples.map {
      case (objRefAttr, operation) =>
        val postmanItem = buildPostmanItem(baseUrl, operation, serviceSpecificHeaders, examplesProvider, None)
        val postmanItemWithTests = addItemTests(postmanItem, None)

        addDependencyItemVarSetting(objRefAttr, postmanItemWithTests)
    }

    val setupFolderWithDependantEntities = setupFolder.copy(
      item = setupFolder.item ++ requiredEntitiesSetupSteps
    )

    PostmanCollection(
      info = collectionInfo,
      item = postmanCollectionFolders.:+(cleanupFolder).+:(setupFolderWithDependantEntities),
      variable = Seq(
        PostmanVariable(key = Variables.BaseUrl, value = Variables.BaseUrlValue, `type` = "string")
      ),
      auth = Some(Auth(
        `type` = AuthType.basic,
        basic = List(
          AuthEntry("username", s"{{${Variables.FlowToken}}}"),
          AuthEntry("password", "")
        )
      ))
    )
  }

  private def generateDependantOperations(resolvedService: ResolvedService): Seq[(ObjectReferenceAttrValue, Operation)] = {

    val service: Service = resolvedService.service
    val serviceNamespaceToResources: Map[String, Seq[Resource]] = resolvedService.serviceNamespaceToResources

    // TODO: think about the model that can have yet another id dependency in the endpoint that creates it - is it already covered?
    def recurOps(typ: String): Seq[ObjectReferenceAttrValue] = {

      service.models.find(_.name == typ) match {
        case Some(model) =>
          model.fields.flatMap {
            case field if service.models.exists(_.name == field.`type`) =>
              recurOps(field.`type`)
            case field =>
              val objRefAttrOpt = field.attributes.collectFirst {
                case attr if attr.name.equalsIgnoreCase(ObjectReferenceAttribute.Key) => attr.value.asOpt[ObjectReferenceAttrValue]
              }.flatten

              objRefAttrOpt match {
                case Some(objAttrRef) =>
                  Seq(objAttrRef)
                case None =>
                  Nil
              }
          }
        case None if service.unions.exists(_.name == typ) =>
          val union = service.unions.find(_.name == typ).get
          union.types.flatMap(u => recurOps(u.`type`))
        case _ =>
          Nil
      }

    }

    val objRefAttrs = service.resources.flatMap { resource =>
      resource.operations.flatMap { operation =>

        operation.body.map { body =>

          val typ = body.`type`
          recurOps(typ)
        }.getOrElse(Nil)
      }
    }

    //  val objRefAttrs = for {
    //    resource <- service.resources
    //    operation <- resource.operations
    //    body <- operation.body
    //    dependantOps <- recurOps(body.`type`)
    //  } yield dependantOps

    val objRefAttrToOperation = objRefAttrs.map { objRefAttr =>
      val operationOpt = serviceNamespaceToResources.getOrElse(objRefAttr.relatedServiceNamespace, Nil)
        .find(_.`type` == objRefAttr.resourceType)
        .map(_.operations).getOrElse(Nil)
        .find(_.method.toString == objRefAttr.operationMethod)

      (objRefAttr, operationOpt)
    }
      .collect {
        case (objRefAttr, Some(operation)) =>
          (objRefAttr, operation)
      }

    objRefAttrToOperation.distinct
  }

  private def buildPostmanItem(
    baseUrl: String,
    operation: Operation,
    serviceSpecificHeaders: Seq[PostmanHeader],
    modelExampleProvider: ExampleJson,
    pathVariableOpt: Option[PathVariable]
  ): PostmanCollectionItem = {
    val postmanRequest = buildPostmanRequest(baseUrl, operation, serviceSpecificHeaders, modelExampleProvider, pathVariableOpt)

    PostmanCollectionItem(
      id = None,
      name = Some(s"${operation.method} ${operation.path}"),
      description = operation.description,
      request = postmanRequest,
      response = buildPostmanExampleResponses(postmanRequest, operation, modelExampleProvider)
    )
  }

  private def addItemTests(item: PostmanCollectionItem, pathVariableOpt: Option[PathVariable]): PostmanCollectionItem = {

    def isParametrizedOnlyWithOrg(item: PostmanCollectionItem): Boolean = {
      import item.request

      request.url.variable.size <= 1 &&
        request.url.variable.headOption.forall(_.value.forall(_ == s"{{${Variables.Organization}}}")) &&
        request.url.query.filterNot(_.disabled).forall(_.value.isDefined)
    }

    def methodEquals(item: PostmanCollectionItem, method: String): Boolean =
      item.request.method.equalsIgnoreCase(method)

    item match {
      case simpleGet if methodEquals(simpleGet, "GET") && isParametrizedOnlyWithOrg(simpleGet) =>
        val test = PredefinedCollectionItems.testEventResponseStatusOk(
          "GET requests parametrized only by Organization should return 2xx"
        )
        item.copy(event = item.event :+ test)
      case simplePost if methodEquals(simplePost, "POST") && isParametrizedOnlyWithOrg(simplePost) =>
        val test = PredefinedCollectionItems.testPostStatusOk(
          "POST requests parametrized only by Organization should return 2xx",
          pathVariableOpt
        )
        item.copy(event = item.event :+ test)
      case other =>
        other
    }
  }

  private def addDependencyItemVarSetting(objRefAttr: ObjectReferenceAttrValue, item: PostmanCollectionItem): PostmanCollectionItem = {

    val scriptExecFragment = Seq(
      """var jsonData = JSON.parse(responseBody);""",
      s"""var id = jsonData["${objRefAttr.identifierField}"];""",
      s"""if (id != null) pm.environment.set("${objRefAttr.toPostmanVariableName}", id);"""
    )

    item.event.find(_.listen == EventType.test) match {
      case Some(testEvent) =>
        val updatedScript = testEvent.script.copy(
          exec = testEvent.script.exec ++ scriptExecFragment
        )
        val updatedTestEvent = testEvent.copy(script = updatedScript)
        val updatedEvents = item.event.filterNot(_.listen == EventType.test) :+ updatedTestEvent
        item.copy(event = updatedEvents)
      case None =>
        val eventToAdd = Event(
          listen = EventType.test,
          script = Script(
            exec = scriptExecFragment
          )
        )
        item.copy(event = item.event :+ eventToAdd)
    }

  }

  private def buildPostmanRequest(
    baseUrl: String,
    operation: Operation,
    serviceSpecificHeaders: Seq[PostmanHeader],
    modelExampleProvider: ExampleJson,
    pathVariableOpt: Option[PathVariable]
  ): PostmanRequest = {
    val protocol = baseUrl.takeWhile(_ != ':')

    // hardcoded fix
    val rawHost = Variables.BaseUrl.stripPrefix(protocol).stripPrefix("://")

    val parameterMap = operation.parameters.groupBy(_.location)

    def getParameters(location: ParameterLocation): Seq[Parameter] =
      parameterMap.getOrElse(location, default = Seq.empty)

    def getDescription(p: Parameter): Option[String] =
      p.description.orElse(Some(s"Type: ${p.`type`}  | Required: ${p.required}"))

    val queryParams = getParameters(ParameterLocation.Query).map { p =>
      PostmanUrlQuery(
        key = p.name,
        value = p.example.orElse(p.default),
        description = getDescription(p),
        disabled = !p.required)
    }

    val headersFromParams = getParameters(ParameterLocation.Header).map { p =>
      PostmanHeader(
        key = p.name,
        value = p.example.orElse(p.default),
        description = getDescription(p)
      )
    }

    val pathParams =
      getParameters(ParameterLocation.Path).map { p =>
        PostmanUrlPathVariable(
          key = p.name,
          value = Some{
            if (pathVariableOpt.filter(_.name == p.name).isDefined)
              s"{{${pathVariableOpt.get.postmanVarName}}}"
            else
              generatePathParamValue(p)
          },
          description = getDescription(p),
          disabled = !p.required)
      }

    val postmanUrl = PostmanRequestUrl(
      raw = s"{{${Variables.BaseUrl}}}" + operation.path,
      protocol = "",
      host = Seq(s"{{${Variables.BaseUrl}}}"),
      path = operation.path.stripPrefix("/").split('/').toSeq,
      query = queryParams,
      variable = pathParams
    )

    val requestBodyOpt = operation.body.flatMap { body =>
      val jsonOpt = modelExampleProvider.sample(body.`type`)
      jsonOpt.map { json =>
        PostmanRequestBodyRaw(Json.prettyPrint(json))
      }
    }

    val headers: Seq[PostmanHeader] = requestBodyOpt.foldLeft(serviceSpecificHeaders ++ headersFromParams) { (headers, _)  =>
      headers :+ PostmanHeader("Content-Type", Some("application/json"), description = Some("Required to send JSON body"))
    }

    PostmanRequest(
      url = postmanUrl,
      method = operation.method.toString,
      description = operation.description,
      headers = headers,
      body = requestBodyOpt
    )
  }

  private def generatePathParamValue(parameter: Parameter): String = {
    parameter match {
      case organizationParam if organizationParam.name == "organization" =>
        s"{{${Variables.Organization}}}"
      case paramWithDefault if paramWithDefault.example.isDefined =>
        paramWithDefault.example.get
      case paramWithExample if paramWithExample.default.isDefined =>
        paramWithExample.default.get
      case _ =>
        "1" // TODO: set this default value according to the type
    }
  }

  private def buildPostmanExampleResponses(
    postmanRequest: PostmanRequest,
    operation: Operation,
    modelExampleProvider: ExampleJson
  ): Seq[PostmanCollectionItemResponse] = {
    operation.responses.flatMap { response =>

      response.code match {
        case ResponseCodeInt(responseCode) =>

          val responseBodyExampleOpt =
            if (response.`type`.equalsIgnoreCase("unit")) None
            else modelExampleProvider.sample(response.`type`).map(Json.prettyPrint)

          val postmanPreviewLangOpt = responseBodyExampleOpt.map(_ => "json")
          val responseTypeSimpleName = {
            val typ = response.`type`
            val startIndex = typ.lastIndexOf('.') + 1
            typ.slice(startIndex, typ.length)
          }

          val responseHeaders = response.headers.map { headers =>
            headers.map { header =>
              PostmanHeader(header.name, header.default, header.description)
            }
          }.getOrElse(Seq.empty)

          val exampleResponse = PostmanCollectionItemResponse(
            id = None,
            name = Some(s"Example $responseCode - $responseTypeSimpleName"),
            originalRequest = Some(postmanRequest),
            header = responseHeaders,
            body = responseBodyExampleOpt,
            `_postman_previewlanguage` = postmanPreviewLangOpt,
            status = None,
            code = responseCode
          )
          Some(exampleResponse)

        case unrecognized =>
          println(s"Unrecognized response code in operation ${operation.method} ${operation.path} examples - $unrecognized. " +
            s"Dropping this example from the result Postman Collection")
          None
      }
    }
  }

  private def writePostmanCollectionToFile(service: Service, fileName: String, postmanCollection: PostmanCollection): File = {

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
