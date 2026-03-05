package generator.openapi.aws

import io.apibuilder.spec.v0.{models => ab}
import sttp.apispec.{ExtensionValue, SecurityScheme}
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

object AwsApiGatewayEnricher {

  private val PathVariablePattern = """\{(\w+)\}""".r

  def enrich(openApi: OpenAPI, config: AwsApiGatewayConfig, service: ab.Service): OpenAPI = {
    val host = serviceHost(service)
    val withValidators = addTopLevelExtensions(openApi)
    val withOperations = enrichPaths(withValidators, config, host)
    config.authorizer.fold(withOperations)(addAuthorizer(withOperations, _, config.requestParameters))
  }

  private def serviceHost(service: ab.Service): String = {
    service.attributes
      .find(_.name == "api-build")
      .flatMap(a => (a.value \ "host").asOpt[String])
      .getOrElse(service.name)
  }

  private def addTopLevelExtensions(openApi: OpenAPI): OpenAPI = {
    val validators = ExtensionValue(
      """|{
         |  "all": {
         |    "validateRequestBody": true,
         |    "validateRequestParameters": true
         |  }
         |}""".stripMargin,
    )
    openApi
      .addExtension("x-amazon-apigateway-request-validators", validators)
  }

  private def enrichPaths(openApi: OpenAPI, config: AwsApiGatewayConfig, host: String): OpenAPI = {
    val paths = openApi.paths
    val enrichedItems = paths.pathItems.map { case (path, pathItem) =>
      path -> enrichPathItem(pathItem, path, config, host)
    }
    openApi.copy(paths = Paths(enrichedItems, paths.extensions))
  }

  private def enrichPathItem(
    pathItem: PathItem,
    path: String,
    config: AwsApiGatewayConfig,
    host: String,
  ): PathItem = {
    pathItem.copy(
      get = pathItem.get.map(enrichOperation(_, path, ab.Method.Get, config, host)),
      put = pathItem.put.map(enrichOperation(_, path, ab.Method.Put, config, host)),
      post = pathItem.post.map(enrichOperation(_, path, ab.Method.Post, config, host)),
      delete = pathItem.delete.map(enrichOperation(_, path, ab.Method.Delete, config, host)),
      patch = pathItem.patch.map(enrichOperation(_, path, ab.Method.Patch, config, host)),
      head = pathItem.head.map(enrichOperation(_, path, ab.Method.Head, config, host)),
      options = pathItem.options.map(enrichOperation(_, path, ab.Method.Options, config, host)),
      trace = pathItem.trace.map(enrichOperation(_, path, ab.Method.Trace, config, host)),
    )
  }

  private def enrichOperation(
    op: Operation,
    path: String,
    method: ab.Method,
    config: AwsApiGatewayConfig,
    host: String,
  ): Operation = {
    val integration = integrationExtension(path, method, config, host)
    op.addExtension("x-amazon-apigateway-integration", integration)
      .addExtension("x-amazon-apigateway-request-validator", ExtensionValue("\"all\""))
  }

  private def integrationExtension(
    path: String,
    method: ab.Method,
    config: AwsApiGatewayConfig,
    host: String,
  ): ExtensionValue = {
    config.integration.`type` match {
      case "mock" => mockIntegration()
      case t @ ("http" | "http_proxy") => httpIntegration(path, method.toString, t, config, host)
      case other => ExtensionValue(s"""{"type": "$other"}""")
    }
  }

  private def mockIntegration(): ExtensionValue = {
    ExtensionValue(
      """|{
         |  "type": "mock",
         |  "requestTemplates": {
         |    "application/json": "{\"statusCode\": 200}"
         |  },
         |  "responses": {
         |    "default": {
         |      "statusCode": "200"
         |    }
         |  }
         |}""".stripMargin,
    )
  }

  private def httpIntegration(
    path: String,
    method: String,
    integrationType: String,
    config: AwsApiGatewayConfig,
    host: String,
  ): ExtensionValue = {
    val uri = s"${config.integration.protocol}://$host.${config.integration.domain}$path"
    val pathParams = PathVariablePattern.findAllMatchIn(path).map(_.group(1)).toSeq

    val entries = Seq.newBuilder[String]
    entries += s""""type": "$integrationType""""
    entries += s""""httpMethod": "$method""""
    entries += s""""uri": "$uri""""
    config.integration.connectionType.foreach(ct => entries += s""""connectionType": "$ct"""")
    config.integration.connectionId.foreach(ci => entries += s""""connectionId": "$ci"""")

    val requestParamEntries = pathParams.map { p =>
      s""""integration.request.path.$p": "method.request.path.$p""""
    } ++ config.requestParameters.map { rp =>
      s""""${rp.to}": "${rp.from}""""
    }

    if (requestParamEntries.nonEmpty) {
      entries += s""""requestParameters": { ${requestParamEntries.mkString(", ")} }"""
    }

    ExtensionValue(s"{ ${entries.result().mkString(", ")} }")
  }

  private def addAuthorizer(
    openApi: OpenAPI,
    authorizer: AwsApiGatewayAuthorizer,
    requestParameters: Seq[AwsApiGatewayRequestParameter],
  ): OpenAPI = {
    val _ = requestParameters // reserved for future use
    val ttlEntry = authorizer.ttl.fold("")(t => s""",\n  "authorizerResultTtlInSeconds": $t""")
    val authorizerExt = ExtensionValue(
      s"""|{
          |  "type": "request",
          |  "authorizerUri": "${authorizer.uri}",
          |  "identitySource": "${authorizer.identitySource}"$ttlEntry
          |}""".stripMargin,
    )

    val secScheme = SecurityScheme(
      `type` = "apiKey",
      name = Some("Authorization"),
      in = Some("header"),
      extensions = ListMap(
        "x-amazon-apigateway-authtype" -> ExtensionValue("\"custom\""),
        "x-amazon-apigateway-authorizer" -> authorizerExt,
      ),
    )

    val components = openApi.components.getOrElse(Components())
    val updatedComponents = components.copy(
      securitySchemes = components.securitySchemes + (authorizer.name -> Right(secScheme)),
    )

    val secReq: ListMap[String, Vector[String]] = ListMap(authorizer.name -> Vector.empty[String])
    openApi.copy(
      components = Some(updatedComponents),
      security = openApi.security :+ secReq,
    )
  }
}
