package controllers

import io.apibuilder.generator.v0.models.json._
import io.apibuilder.generator.v0.models.{Invocation, InvocationForm}
import lib.Validation
import play.api.libs.json._
import play.api.mvc._

class Invocations extends InjectedController {

  def postByKey(key: String): Action[JsValue] = Action(parse.json) { request =>
      Generators.findGenerator(key).map(_.generator) match {
        case None => NotFound
        case Some(generator) =>
          request.body.validate[InvocationForm] match {
            case e: JsError => Conflict(Json.toJson(Validation.invalidJson(e)))
            case s: JsSuccess[InvocationForm] => {
              generator.invoke(s.get) match {
                case Left(errors) => Conflict(Json.toJson(Validation.errors(errors)))
                case Right(sourceFiles) =>
                  // Also send back single source for backwards compatibility
                  val singleSource = sourceFiles.map(_.contents).mkString("\n\n").trim
                  Ok(Json.toJson(Invocation(singleSource, sourceFiles)))
              }
            }
          }
      }
  }

}
