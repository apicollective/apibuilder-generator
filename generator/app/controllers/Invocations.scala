package controllers

import com.gilt.apidoc.generator.v0.models.json._
import com.gilt.apidoc.generator.v0.models.{Invocation, InvocationForm, Generator}
import generator.{CodeGenTarget, CodeGenerator}
import lib.Validation
import play.api.libs.json._
import play.api.mvc._

object Invocations extends Controller {
  def postByKey(key: String) = Action(parse.json(maxLength = 1024 * 1024)) { request: Request[JsValue] =>
    Generators.findGenerator(key) match {
      case Some((_, generator)) =>
        request.body.validate[InvocationForm] match {
          case e: JsError => Conflict(Json.toJson(Validation.invalidJson(e)))
          case s: JsSuccess[InvocationForm] => Ok(Json.toJson(Invocation(generator.invoke(s.get))))
        }
      case _ => NotFound
    }
  }
}
