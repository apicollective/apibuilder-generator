package generator

import examples.ExampleJson
import generator.Utils.Description
import io.apibuilder.spec.v0.models.{Operation, ResponseCodeInt}
import io.flow.postman.collection.v210.v0.{models=>postman}
import play.api.Logging
import play.api.libs.json.Json

object PostmanExampleResponseBuilder extends Logging {

  def build(
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

          val responseTypeSimpleName = {
            val typ = response.`type`
            val startIndex = typ.lastIndexOf('.') + 1
            typ.slice(startIndex, typ.length)
          }

          val responseHeaders = response.headers.map { headers =>
            headers.map { header =>
              postman.Header(
                key = header.name,
                value = header.default.getOrElse(""),
                disabled = None,
                description = header.description.map(Description(_)))
            }
          }.getOrElse(Seq.empty)

          val exampleResponse = postman.Response(
            id = None,
            name = Some(s"Example $responseCode - $responseTypeSimpleName"),
            originalRequest = Some(postmanRequest),
            responseTime = None,
            header = Some(responseHeaders),
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

}
