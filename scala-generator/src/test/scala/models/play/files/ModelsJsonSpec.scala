package scala.models.play.files

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import io.apibuilder.spec.v0.models.{Apidoc, Application, Info, Organization, Service}
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.models.play.Helpers.compareWithoutWhiteSpaces

class ModelsJsonSpec extends FunSpec with Matchers {
  val basicService = Service(
    apidoc = Apidoc("0"),
    name = "name",
    organization = Organization("foo"),
    application = Application("bar"),
    namespace = "x",
    version = "1",
    baseUrl = None,
    description = None,
    info = Info(),
    headers = Nil,
    imports = Nil,
    enums = Nil,
    unions = Nil,
    models = Nil,
    resources = Nil,
    attributes = Nil,
    annotations = Nil
  )

  it("generates models and joda json") {
    val form = InvocationForm(
      service = basicService,
      attributes = Seq(Attribute("time", "joda"))
    )
    ModelsJson.contents(form).replaceAll(" +", " ").replaceAll(" +\n", "\n").trim shouldBe
      """package x.models
        |
        | package object json {
        |
        | import play.api.libs.json.{__, JsString, Writes}
        | import play.api.libs.functional.syntax._
        |
        | import play.api.libs.json.Writes._
        |import play.api.libs.json.Reads._
        |import play.api.libs.json.JodaReads.DefaultJodaDateTimeReads
        |import play.api.libs.json.JodaReads.DefaultJodaLocalDateReads
        |import play.api.libs.json.JodaWrites.JodaDateTimeWrites
        |import play.api.libs.json.JodaWrites.DefaultJodaLocalDateWrites
        |
        | import x.models.json._
        |
        |
        |
        |
        | }""".stripMargin
  }

  it("generates models and java json") {
    val form = InvocationForm(
      service = basicService,
      attributes = Nil
    )
    ModelsJson.contents(form).replaceAll(" +", " ").replaceAll(" +\n", "\n").trim shouldBe
      """package x.models
        |
        | package object json {
        |
        | import play.api.libs.json.{__, JsString, Writes}
        | import play.api.libs.functional.syntax._
        |
        | import play.api.libs.json.Writes._
        |import play.api.libs.json.Reads._
        |
        | import x.models.json._
        |
        |
        |
        |
        | }""".stripMargin
  }
}
