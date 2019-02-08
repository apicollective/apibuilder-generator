package generator

import examples.ExampleJson
import generator.Heuristics.PathVariable
import generator.PostmanCollectionGenerator.Variables
import generator.Utils.Description
import io.apibuilder.spec.v0.models._
import io.flow.postman.collection.v210.v0.{models => postman}
import play.api.Logging
import play.api.libs.json.Json

object PostmanItemBuilder extends Logging {

  def build(
    baseUrl: String,
    operation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    modelExampleProvider: ExampleJson,
    pathVariableOpt: Option[PathVariable]
  ): postman.Item = {
    val postmanRequest = buildPostmanRequest(baseUrl, operation, serviceSpecificHeaders, modelExampleProvider, pathVariableOpt)

    postman.Item(
      id = None,
      name = Some(s"${operation.method} ${operation.path}"),
      description = operation.description.map(Description(_)),
      request = postmanRequest,
      response = Some(buildExampleResponses(postmanRequest, operation, modelExampleProvider))
    )
  }

  private def buildPostmanRequest(
    baseUrl: String,
    operation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    modelExampleProvider: ExampleJson,
    pathVariableOpt: Option[PathVariable]
  ): postman.Request = {
    val protocol = baseUrl.takeWhile(_ != ':')

    // hardcoded fix
    val rawHost = Variables.BaseUrl.stripPrefix(protocol).stripPrefix("://")

    val parameterMap = operation.parameters.groupBy(_.location)

    def getParameters(location: ParameterLocation): Seq[Parameter] =
      parameterMap.getOrElse(location, default = Seq.empty)

    def getDescription(p: Parameter): Option[String] =
      p.description.orElse(Some(s"Type: ${p.`type`}  | Required: ${p.required}"))

    val queryParams = getParameters(ParameterLocation.Query).map { p =>
      postman.QueryParam(
        key = Some(p.name),
        value = p.example.orElse(p.default),
        description = getDescription(p).map(Description(_)),
        disabled = Some(!p.required))
    }

    val headersFromParams = getParameters(ParameterLocation.Header).map { p =>
      postman.Header(
        key = p.name,
        value = p.example.orElse(p.default).getOrElse(""),
        description = getDescription(p).map(Description(_))
      )
    }

    val pathParams =
      getParameters(ParameterLocation.Path).map { p =>
        postman.Variable(
          key = Some(p.name),
          value = Some{
            if (pathVariableOpt.filter(_.name == p.name).isDefined)
              s"{{${pathVariableOpt.get.postmanVarName}}}"
            else
              generatePathParamValue(p)
          },
          description = getDescription(p).map(Description(_)),
          disabled = Some(!p.required))
      }

    val postmanUrl = postman.Url(
      raw = Some(s"{{${Variables.BaseUrl}}}" + operation.path),
      protocol = None,
      host = Some(Seq(s"{{${Variables.BaseUrl}}}")),
      path = Some(operation.path.stripPrefix("/").split('/').toSeq),
      query = Some(queryParams),
      variable = Some(pathParams)
    )

    val requestBodyOpt = operation.body.flatMap { body =>
      val jsonOpt = modelExampleProvider.sample(body.`type`)
      jsonOpt.map { json =>
        postman.Body(
          Some(Json.prettyPrint(json)),
          mode = Some(postman.BodyMode.Raw)
        )
      }
    }

    val headers: Seq[postman.Header] = operation.body.foldLeft(serviceSpecificHeaders ++ headersFromParams) { (headers, _)  =>
      headers :+ postman.Header("Content-Type", "application/json", description = Some(Description("Required to send JSON body")))
    }

    postman.Request(
      url = Option(postmanUrl),
      method = Option(postman.Method(operation.method.toString)),
      description = operation.description.map(Description(_)),
      auth = None,
      header = Some(headers),
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

  private def buildExampleResponses(
    postmanRequest: postman.Request,
    operation: Operation,
    modelExampleProvider: ExampleJson
  ): Seq[postman.Response] = {
    operation.responses.flatMap { response =>

      response.code match {
        case ResponseCodeInt(responseCode) =>

          val responseBodyExampleOpt =
            if (response.`type`.equalsIgnoreCase("unit")) None
            else modelExampleProvider.sample(response.`type`).map(Json.prettyPrint)

          val responseTypeSimpleName = extractSimpleName(response)

          val responseHeadersOpt = response.headers.map { headers =>
            headers.map { header =>
              postman.Header(
                key = header.name,
                value = header.default.getOrElse(""),
                disabled = None,
                description = header.description.map(Description(_)))
            }
          }

          val exampleResponse = postman.Response(
            id = None,
            name = Some(s"Example $responseCode - $responseTypeSimpleName"),
            originalRequest = Some(postmanRequest),
            responseTime = None,
            header = responseHeadersOpt,
            body = responseBodyExampleOpt,
            status = None,
            code = Some(responseCode)
          )
          Some(exampleResponse)

        case unrecognized =>
          logger.warn(s"Unrecognized response code in operation ${operation.method} ${operation.path} examples - $unrecognized. " +
            s"Dropping this example from the result Postman Collection")
          None
      }
    }
  }

  private def extractSimpleName(response: Response): String = {
    val typ = response.`type`
    val startIndex = typ.lastIndexOf('.') + 1
    typ.slice(startIndex, typ.length)
  }

}
