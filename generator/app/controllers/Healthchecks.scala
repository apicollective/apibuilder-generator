package controllers

import play.api.libs.json.*
import play.api.mvc.*

class Healthchecks extends InjectedController {

  private val Result = Json.toJson(Map("status" -> "healthy"))

  def get() = Action {
    Ok(Result)
  }

}
