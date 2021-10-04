import com.bryzek.apibuilder.union.of.unions.v0.models._
import com.bryzek.apibuilder.union.of.unions.v0.models.json._
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json._

import java.util.UUID

class SerializationSpec extends PlaySpec with GuiceOneAppPerSuite {

  private[this] def mustParse[T](value: T)(implicit writes: Writes[T], reads: Reads[T]) = {
    Json.parse(Json.toJson(value).toString()).validate[T] match {
      case JsSuccess(value, _) => value
      case e: JsError          => sys.error(s"Failed to parse: ${e.errors.mkString(", ")}")
    }
  }

  "group" must {
    val name = "test"
    val group = Group(name = name)

    "toJson" in {
      Json.toJson(group) mustBe Json.obj(
        "name" -> name,
        "discriminator" -> "group",
      )
    }

    "fromJson" in {
      mustParse[Group](group) match {
        case g: Group => g mustBe group
        case other => fail(s"Parsed as class ${other.getClass} when Group expected")
      }
    }
  }

  "guest_user" must {
    val guestUser = GuestUser()

    "toJson" in {
      Json.toJson(guestUser) mustBe Json.obj(
        "discriminator" -> "guest_user",
      )
    }

    "fromJson" in {
      mustParse[GuestUser](guestUser) match {
        case g: GuestUser => g mustBe guestUser
        case other => fail(s"Parsed as class ${other.getClass} when Group expected")
      }
    }
  }

  "registered_user" must {
    val email = "test@apibuilder.io"
    val guid = UUID.randomUUID()
    val registeredUser = RegisteredUser(guid = guid, email = email)

    "toJson" in {
      Json.toJson(registeredUser) mustBe Json.obj(
        "guid" -> guid,
        "email" -> email,
        "discriminator" -> "registered_user",
      )
    }

    "fromJson" in {
      mustParse[RegisteredUser](registeredUser) match {
        case u: RegisteredUser => u mustBe registeredUser
        case other => fail(s"Parsed as class ${other.getClass} when Group expected")
      }
    }
  }

  "party" must {
    val party = GuestUser(email = None)

    "toJson" in {
      Json.toJson(party) mustBe Json.obj(
        "discriminator" -> "guest_user",
      )
    }

    "fromJson" in {
      mustParse[Party](party) match {
        case g: GuestUser => g mustBe party
        case other => fail(s"Parsed as class ${other.getClass} when GuestUser expected")
      }
    }
  }
}
