package lib

import io.apibuilder.generator.v0.models.json._
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Results.{BadRequest, InternalServerError}
import play.api.mvc.{RequestHeader, Result}
import scala.concurrent.Future

class ErrorHandler
  extends play.api.http.HttpErrorHandler {

  override def onClientError(request: RequestHeader, statusCode: Int, message: String = ""): Future[Result] = {
    Future.successful(BadRequest(Json.toJson(Validation.serverError("Bad Request"))))
  }

  def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    Logger.error(exception.toString, exception)
    Future.successful(InternalServerError(Json.toJson(Validation.serverError())))
  }

}
