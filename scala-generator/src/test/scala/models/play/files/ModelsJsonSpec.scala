package scala.models.play.files

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}

import scala.models.play.Helpers
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ModelsJsonSpec extends AnyFunSpec with Matchers {

  it("generates models and joda json") {
    val form = InvocationForm(
      service = Helpers.basicService("x"),
      attributes = Seq(Attribute("scala_generator.time_library", "joda"))
    )
    ModelsJson.contents(form, scala3Support = false).replaceAll(" +", " ").replaceAll(" +\n", "\n").trim shouldBe
      """package x.models
        |
        | package object json {
        |
        | import play.api.libs.json.{__, JsString, Writes}
        | import play.api.libs.functional.syntax.*
        |
        | import play.api.libs.json.Writes.*
        |import play.api.libs.json.Reads.*
        |import play.api.libs.json.JodaReads.DefaultJodaDateTimeReads
        |import play.api.libs.json.JodaWrites.JodaDateTimeWrites
        |import play.api.libs.json.JodaReads.DefaultJodaLocalDateReads
        |import play.api.libs.json.JodaWrites.DefaultJodaLocalDateWrites
        |
        | import x.models.json.*
        |
        |
        |
        |
        | }""".stripMargin
  }

  it("generates models and java json") {
    val form = InvocationForm(
      service = Helpers.basicService("x"),
      attributes = Nil
    )
    ModelsJson.contents(form, scala3Support = false).replaceAll(" +", " ").replaceAll(" +\n", "\n").trim shouldBe
      """package x.models
        |
        | package object json {
        |
        | import play.api.libs.json.{__, JsString, Writes}
        | import play.api.libs.functional.syntax.*
        |
        | import play.api.libs.json.Writes.*
        |import play.api.libs.json.Reads.*
        |
        | import x.models.json.*
        |
        |
        |
        |
        | }""".stripMargin
  }
}
