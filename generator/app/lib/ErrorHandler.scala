package lib

import io.apibuilder.generator.v0.models.json._
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.Results.{BadRequest, InternalServerError}
import play.api.mvc.{RequestHeader, Result}

import scala.concurrent.Future

class ErrorHandler
  extends play.api.http.HttpErrorHandler with Logging {

  override def onClientError(request: RequestHeader, statusCode: Int, message: String = ""): Future[Result] = {
    logger.warn(s"client error - req[$request] statusCode[$statusCode] message[$message]")
    logger.warn(request.headers.toString)
    Future.successful(BadRequest(Json.toJson(Validation.serverError("Bad Request"))))
  }

  def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    logger.error(exception.toString, exception)
    Future.successful(InternalServerError(Json.toJson(Validation.serverError())))
  }

}
