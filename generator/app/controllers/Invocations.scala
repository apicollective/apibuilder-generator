package controllers

import com.bryzek.apidoc.generator.v0.models.json._
import com.bryzek.apidoc.generator.v0.models.{Invocation, InvocationForm}
import lib.Validation
import play.api.libs.json._
import play.api.mvc._

object Invocations extends Controller {

  def postByKey(key: String) = Action(parse.json(maxLength = 1024 * 1024)) { request: Request[JsValue] =>
    Generators.findGenerator(key) match {
      case Some((target, generator)) =>
        request.body.validate[InvocationForm] match {
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
