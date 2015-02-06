package controllers

import com.gilt.apidoc.example.union.types.v0.models.{Foo, GuestUser, RegisteredUser, User, UuidWrapper}
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
          email = email,
          preference = Foo.A
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

  "POST /users with a UUID" in new WithServer {
    val guid = UUID.randomUUID

    await(
      client.users.post(UuidWrapper(guid))
    ) match {
      case wrapper: UuidWrapper => {
        wrapper.value must be(guid)
      }
      case user => {
        fail("Creating a uuid wrapper returning a user w/ invalid type: " + user)
      }
    }
  }

  "GET /users" in new WithServer {
    await(client.users.get()).size must be(3)
  }

  "GET /users/:guid" in new WithServer {
    val userGuids = await(
      client.users.get()
    ).map { user =>
      user match {
        case user: RegisteredUser => user.guid
        case user: GuestUser => user.guid
        case wrapper: UuidWrapper => wrapper.value
      }
    }

    userGuids.foreach { userGuid =>
      await(client.users.getByGuid(userGuid)).getOrElse {
        fail("failed to find user by guid")
      } match {
        case user: RegisteredUser => user.guid must be(userGuid)
        case user: GuestUser => user.guid must be(userGuid)
        case wrapper: UuidWrapper => wrapper.value must be(userGuid)
      }
    }

    await(client.users.getByGuid(UUID.randomUUID)) must be(None)
  }

}
