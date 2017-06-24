package controllers

import io.apibuilder.example.union.types.discriminator.v0.models._
import io.apibuilder.example.union.types.discriminator.v0.models.json._
import play.api.mvc._
import play.api.libs.json._

object Users extends Controller {

  private[this] val users: Seq[User] = Seq(
    RegisteredUser("usr-1", "registered@test.apidoc.me"),
    GuestUser("usr-2"),
    UserString("usr-3"),
    SystemUser.System,
    SystemUser.Anonymous
  )

  def get() = Action {
    Ok(Json.toJson(users))
  }

  def getById(id: String) = Action {
    users.find { u =>
      u match {
        case RegisteredUser(userId, _) => userId == id
        case GuestUser(userId, _) => userId == id
        case UserString(userId) => userId == id
        case UserUndefinedType(_) => false
        case SystemUser.System => SystemUser.System.toString == id
        case SystemUser.Anonymous => SystemUser.Anonymous.toString == id
        case SystemUser.UNDEFINED(name) => false
      }
    } match {
      case None => {
        NotFound
      }
      case Some(u) => {
        Ok(
          Json.toJson(u)
        )
      }
    }
  }

  /**
   * Example:
   * curl -X POST -H "Content-Type: application/json" -d "{ \"discriminator\": \"user\", \"email\": \"testing@mailinator.com\" } }" http://localhost:7100/users
   */
  def post() = Action(parse.json) { request =>
    request.body.validate[User] match {
      case e: JsError => {
        BadRequest(Json.obj("message" -> s"invalid json: ${e}"))
      }
      case s: JsSuccess[User] => {
        val user = s.get

        user match {
          case RegisteredUser(id, email) => println(s"Received Registered User $id")
          case GuestUser(id, email) => println(s"Received Guest User $id")
          case UserString(id) => println(s"Received User String $id")
          case UserUndefinedType(name) => println(s"Received undefined type $name")
          case SystemUser.System => println(s"Received SystemUser.System")
          case SystemUser.Anonymous => println(s"Received SystemUser.Anonymous")
          case SystemUser.UNDEFINED(name) => println(s"Received SystemUser.UNDEFINED($name)")
        }

        Created(Json.toJson(user))
      }
    }
  }

}
