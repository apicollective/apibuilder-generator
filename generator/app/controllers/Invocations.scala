package controllers

import io.apibuilder.generator.v0.models.json._
import io.apibuilder.generator.v0.models.{Invocation, InvocationForm}
import lib.{ServiceApidocBug, Validation}
import play.api.libs.json._
import play.api.mvc._

class Invocations extends InjectedController {

  def getByKey(key: String): Action[AnyContent] = Action { _ =>
    Conflict(Json.toJson(Validation.error(
      s"Use HTTPS POST (not GET) to invoke the generator with key '$key'"
    )))
  }

  def postByKey(key: String): Action[AnyContent] = Action { request =>
    request.body.asJson match {
      case None => Conflict(Json.toJson(Validation.error("Must provide form data (JSON)")))
      case Some(incomingJs) => {
        val js = ServiceApidocBug.rewrite(incomingJs)
        Generators.findGenerator(key).map(_.generator) match {
          case Some(generator) =>
            js.validate[InvocationForm] match {
              case e: JsError => Conflict(Json.toJson(Validation.invalidJson(e)))
              case s: JsSuccess[InvocationForm] => {
                val form = s.get
                generator.invoke(form) match {
                  case Left(errors) => Conflict(Json.toJson(Validation.errors(errors)))
                  case Right(sourceFiles) =>
                    // Also send back single source for backwards compatibility
                    val singleSource = sourceFiles.map(_.contents).mkString("\n\n").trim
                    Ok(Json.toJson(Invocation(singleSource, sourceFiles)))
                }
              }
            }
          case _ => NotFound
        }
      }
    }
  }

}
