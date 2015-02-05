package controllers

import com.gilt.apidoc.example.union.types.v0.models.{GuestUser, RegisteredUser, User}
import com.gilt.apidoc.example.union.types.v0.models.json._
import play.api.mvc._
import play.api.libs.json._
import java.util.UUID

object Users extends Controller {

  private val users: Seq[User] = Seq(
    RegisteredUser(UUID.randomUUID(), "registered@test.apidoc.me"),
    GuestUser(UUID.randomUUID(), "guest@test.apidoc.me")
  )

  def get() = Action {
    Ok(Json.toJson(users))
  }

  def getByGuid(guid: UUID) = Action {
    users.find { u =>
      u match {
        case user: RegisteredUser => user.guid == guid
        case user: GuestUser => user.guid == guid
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
    println("POST")
    request.body.validate[User] match {
      case e: JsError => {
        println(request.body)
        println("BAS JSON:" + e)
        BadRequest(Json.obj("message" -> s"invalid json: ${e}"))
      }
      case s: JsSuccess[User] => {
        val user = s.get

        user match {
          case u: RegisteredUser => println("Received Registered User: " + u)
          case u: GuestUser => println("Received Guest User: " + u)
        }

        Created(Json.toJson(user))
      }
    }
  }

}
