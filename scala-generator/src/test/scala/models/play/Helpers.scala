package scala.models.play

import io.apibuilder.spec.v0.models.{Apidoc, Application, Info, Organization, Service}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

object Helpers extends Matchers {
  def removeAllExtraWhiteSpaces(a: String): String = a.replaceAll("\\s+", " ").trim

  def compareWithoutWhiteSpaces(a: String, b: String): Assertion =
    removeAllExtraWhiteSpaces(a) should be(removeAllExtraWhiteSpaces(b))

  def basicService(namespace: String): Service = Service(
    apidoc = Apidoc("0"),
    name = "name",
    organization = Organization("foo"),
    application = Application("bar"),
    namespace = namespace,
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
}
