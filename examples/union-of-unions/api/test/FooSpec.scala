import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import com.bryzek.apibuilder.union.of.unions.v0.models._
import com.bryzek.apibuilder.union.of.unions.v0.models.json._
import play.api.libs.json.{JsError, JsSuccess, Json}

class FooSpec extends PlaySpec with GuiceOneAppPerSuite {

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
      Json.parse(Json.toJson(group).toString()).validate[Party] match {
        case JsSuccess(value, _) => value match {
          case g: Group => g mustBe group
          case other => fail(s"Parsed as class ${other.getClass} when Group expected")
        }
        case e: JsError         => fail(s"Failed to validate: ${e.errors.mkString(", ")}")
      }
    }
  }

  "guest_user" must {
    val name = "test"
    val guestUser = GuestUser()

    "toJson" in {
      Json.toJson(guestUser) mustBe Json.obj(
        "discriminator" -> "guest_user",
      )
    }

    "fromJson" in {
      Json.parse(Json.toJson(guestUser).toString()).validate[GuestUser] match {
        case JsSuccess(value, _) => value match {
          case g: GuestUser => g mustBe guestUser
          case other => fail(s"Parsed as class ${other.getClass} when Group expected")
        }
        case e: JsError         => fail(s"Failed to validate: ${e.errors.mkString(", ")}")
      }
    }
  }

  "party" must {
    val party: Party = GuestUser(email = None)

    "toJson" in {
      Json.toJson(party) mustBe Json.obj(
        "discriminator" -> "guest_user",
      )
    }

    "fromJson" in {
      Json.parse(Json.toJson(party).toString()).validate[Party] match {
        case JsSuccess(value, _) => value match {
          case g: GuestUser => g mustBe party
          case other => fail(s"Parsed as class ${other.getClass} when GuestUser expected")
        }
        case e: JsError         => fail(s"Failed to validate: ${e.errors.mkString(", ")}")
      }
    }
  }
}
