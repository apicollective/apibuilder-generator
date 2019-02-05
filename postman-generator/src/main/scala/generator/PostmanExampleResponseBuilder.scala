package generator

import examples.ExampleJson
import io.apibuilder.spec.v0.models.{Operation, ResponseCodeInt}
import models.postman.{PostmanCollectionItemResponse, PostmanHeader, PostmanRequest}
import play.api.Logging
import play.api.libs.json.Json

object PostmanExampleResponseBuilder extends Logging {

  def build(
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
          logger.warn(s"Unrecognized response code in operation ${operation.method} ${operation.path} examples - $unrecognized. " +
            s"Dropping this example from the result Postman Collection")
          None
      }
    }
  }

}
