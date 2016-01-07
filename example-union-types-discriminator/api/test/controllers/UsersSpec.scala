package controllers

import com.bryzek.apidoc.example.union.types.discriminator.v0.Client
import com.bryzek.apidoc.example.union.types.discriminator.v0.errors.UnitResponse
import com.bryzek.apidoc.example.union.types.discriminator.v0.models._
import com.bryzek.apidoc.example.union.types.discriminator.v0.models.json._

import java.util.UUID
import scala.util.{Failure, Success, Try}
import play.api.libs.ws._
import play.api.test._

class UsersSpec extends PlaySpecification {

  import scala.concurrent.ExecutionContext.Implicits.global

  val port = 9010
  lazy val client = new Client(s"http://localhost:$port")

  "POST /users with a guest user" in new WithServer(port = port) {
    val id = s"usr-${UUID.randomUUID}"
    val email = s"guest-$id@test.apidoc.me"

    await(
      client.users.post(
        GuestUser(
          id = id,
          email = Some(email)
        )
      )
    ) match {
      case user: GuestUser => {
        user.id must beEqualTo(id)
        user.email must beEqualTo(Some(email))
      }
      case user => {
        failure("Creating a guest user returning a user w/ invalid type: " + user)
      }
    }
  }

  "POST /users with a registered user" in new WithServer(port = port) {
    val id = s"usr-${UUID.randomUUID}"
    val email = s"registered-$id@test.apidoc.me"

    await(
      client.users.post(
        RegisteredUser(
          id = id,
          email = email
        )
      )
    ) match {
      case user: RegisteredUser => {
        user.id must beEqualTo(id)
        user.email must beEqualTo(email)
      }
      case user => {
        failure("Creating a registered user returning a user w/ invalid type: " + user)
      }
    }
  }

  "GET /users" in new WithServer(port = port) {
    await(client.users.get()).size must beEqualTo(2)
  }

  "GET /users/:id" in new WithServer(port = port) {
    val userIds = await(
      client.users.get()
    ).flatMap { user =>
      user match {
        case RegisteredUser(id, email) => Some(id)
        case GuestUser(id, email) => Some(id)
        case UserUndefinedType(name) => None
      }
    }

    userIds.foreach { userId =>
      await(client.users.getById(userId)) match {
        case RegisteredUser(id, email) => id must beEqualTo(userId)
        case GuestUser(id, email) => id must beEqualTo(userId)
        case UserUndefinedType(name) => failure("Should not have received undefined type")
      }
    }

    expectStatus(404) {
      await(client.users.getById(s"usr-${UUID.randomUUID}"))
    }
  }

  def expectStatus(code: Int)(f: => Unit) {
    Try(
      f
    ) match {
      case Success(response) => {
        org.specs2.execute.Failure(s"Expected HTTP[$code] but got HTTP 2xx")
      }
      case Failure(ex) => ex match {
        case UnitResponse(code) => {
          org.specs2.execute.Success()
        }
        case e => {
          org.specs2.execute.Failure(s"Unexpected error: $e")
        }
      }
    }
  }

}
