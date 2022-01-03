package controllers

import io.apibuilder.example.union.types.v0.models._
import io.apibuilder.example.union.types.v0.models.json._
import play.api.mvc._
import play.api.libs.json._
import java.util.UUID

object Users extends Controller {

  private[this] val users: Seq[User] = Seq(
    RegisteredUser(UUID.randomUUID(), "registered@test.apidoc.me", preference = Foo.A),
    GuestUser(UUID.randomUUID(), "guest@test.apidoc.me"),
    UserUuid(UUID.randomUUID())
  )

  def get() = Action {
    Ok(Json.toJson(users))
  }

  def getByGuid(guid: UUID) = Action {
    users.find { u =>
      u match {
        case RegisteredUser(id, email, preference) => id == guid
        case GuestUser(id, email) => id == guid
        case UserUuid(value) => value == guid
        case UserUndefinedType(name) => false
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
   * curl -X POST -H "Content-Type: application/json" -d "{ \"guest_user\": { \"guid\": \"893ac5c5-2ea3-4b0d-8f82-ca8d73a26211\", \"email\": \"testing@mailinator.com\" } }" http://localhost:7100/users
   */
  def post() = Action(parse.json) { request =>
    request.body.validate[User] match {
      case e: JsError => {
        BadRequest(Json.obj("message" -> s"invalid json: ${e}"))
      }
      case s: JsSuccess[User] => {
        val user = s.get

        user match {
          case RegisteredUser(id, email, preference) => println(s"Received Registered User $id email[$email] preference[$preference]")
          case GuestUser(id, email) => println(s"Received Guest User $id email[$email]")
          case UserUuid(value) => println(s"Received UUID $value")
          case UserUndefinedType(name) => {
            sys.error(s"Received undefined type $name")
          }
        }
        Created(Json.toJson(user))
      }
    }
  }

}
