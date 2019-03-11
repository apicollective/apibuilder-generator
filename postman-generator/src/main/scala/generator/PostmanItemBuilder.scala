package generator

import examples.ExampleJson
import generator.Heuristics.PathVariable
import generator.PostmanCollectionGenerator.Constants
import generator.Utils.Description
import io.apibuilder.spec.v0.models._
import io.flow.postman.v0.{models => postman}
import models.attributes.PostmanAttributes
import org.scalactic.TripleEquals._
import play.api.Logging
import play.api.libs.json.Json

object PostmanItemBuilder extends Logging {

  def build(
    operation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    modelExampleProvider: ExampleJson,
    pathVariableOpt: Option[PathVariable]
  ): postman.Item = {
    val postmanRequest = buildPostmanRequest(operation, serviceSpecificHeaders, modelExampleProvider, pathVariableOpt)

    postman.Item(
      id = None,
      name = Some(s"${operation.method} ${operation.path}"),
      description = operation.description.map(Description(_)),
      request = postmanRequest,
      response = Some(buildExampleResponses(postmanRequest, operation, modelExampleProvider))
    )
  }

  private def buildPostmanRequest(
    operation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    modelExampleProvider: ExampleJson,
    pathVariableOpt: Option[PathVariable]
  ): postman.Request = {

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
          value = Some {
            if (pathVariableOpt.exists(_.name === p.name))
              pathVariableOpt.get.postmanVarRef
            else
              generatePathParamValue(p)
          },
          description = getDescription(p).map(Description(_)),
          disabled = Some(!p.required))
      }

    val postmanUrl = postman.Url(
      raw = Some(s"{{${Constants.BaseUrl}}}" + operation.path),
      protocol = None,
      host = Some(Seq(s"{{${Constants.BaseUrl}}}")),
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
    val defaults = List(parameter.example, parameter.default).flatten
    defaults.headOption.getOrElse {
      PostmanAttributes.postmanVariableNameFrom(parameter).reference
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
