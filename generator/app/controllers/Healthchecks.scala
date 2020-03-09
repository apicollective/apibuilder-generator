package controllers

import play.api.libs.json._
import play.api.mvc._

class Healthchecks extends InjectedController {

  private[this] val Result = Json.toJson(Map("status" -> "healthy"))

  def get() = Action {
    Ok(Result)
  }

}
