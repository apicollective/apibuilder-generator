package controllers

import com.gilt.apidoc.example.union.types.v0.models.{GuestUser, RegisteredUser, User}
import com.gilt.apidoc.example.union.types.v0.models.json._
import java.util.UUID

import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._

class UsersSpec extends PlaySpec with OneServerPerSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit override lazy val port = 9010
  implicit override lazy val app: FakeApplication = FakeApplication()
  lazy val client = new com.gilt.apidoc.example.union.types.v0.Client(s"http://localhost:$port")

  "POST /users with a guest user" in new WithServer {
    val guid = UUID.randomUUID
    val email = s"guest-$guid@test.apidoc.me"

    await(
      client.users.post(
        GuestUser(
          guid = guid,
          email = email
        )
      )
    ) match {
      case user: GuestUser => {
        user.guid must be(guid)
        user.email must be(email)
      }
      case user => {
        fail("Creating a guest user returning a user w/ invalid type: " + user)
      }
    }
  }

  "POST /users with a registered user" in new WithServer {
    val guid = UUID.randomUUID
    val email = s"registered-$guid@test.apidoc.me"

    await(
      client.users.post(
        RegisteredUser(
          guid = guid,
          email = email
        )
      )
    ) match {
      case user: RegisteredUser => {
        user.guid must be(guid)
        user.email must be(email)
      }
      case user => {
        fail("Creating a registered user returning a user w/ invalid type: " + user)
      }
    }
  }

  "GET /users" in new WithServer {
    val users = await(
      client.users.get()
    )

    users.size must be(2)
  }

  "GET /users/:guid" in new WithServer {
    val userGuid = await(
      client.users.get()
    ).headOption.getOrElse {
      fail("No users returned")
    } match {
      case user: RegisteredUser => user.guid
      case user: GuestUser => user.guid
    }

    await(client.users.getByGuid(userGuid)).getOrElse {
      fail("failed to find user by guid")
    } match {
      case user: RegisteredUser => user.guid must be(userGuid)
      case user: GuestUser => user.guid must be(userGuid)
    }

    await(client.users.getByGuid(UUID.randomUUID)) must be(None)
  }

}
