package scala.models.play

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Application, Organization, Service}
import org.scalatest.{FunSpec, Matchers}
import scala.models.play.Helpers.compareWithoutWhiteSpaces

class Play26GeneratorSpec extends FunSpec with Matchers {

  it("prependHeader should prepend a header") {
    val contents = "CONTENT"
    val form = InvocationForm(Service(null, null, null, null, null, "", null, info = null))
    val expected = s"""
        HEADER

        CONTENT
    """
    val result = Play26Generator.prependHeader(contents, form, _ => "HEADER")

    compareWithoutWhiteSpaces(result, expected)
  }

  it("file should wrap generator.ServiceFileNames.toFile") {
    val namespace = "namespace"
    val organization = "organization"
    val application = "application"
    val version = "version"
    val suffix = "suffix"
    val contents = "contents"
    val extension = Some("extension")

    val expected = generator.ServiceFileNames.toFile(namespace, organization, application, version, suffix, contents, extension)
    val result = Play26Generator.file(InvocationForm(Service(null, null, Organization(organization), Application(application), namespace, version, info = null)), suffix, contents, extension)

    result should be(expected)
  }

  ignore("formatScala should format valid scala code") {
    val contents = "case class Foo(bar: String)"
    val result = Play26Generator.formatScala(contents)

    result should be('right)
  }

  ignore("formatScala should fail to format invalid scala code") {
    val contents = "Foo Bar"
    val result = Play26Generator.formatScala(contents)

    result should be('left)
  }

  it("formatRoutes should format routes") {
    val contents = """
    A
    B
    C
    """
    val expected = "A\nB\nC"
    val result = Play26Generator.formatRoutes(contents)

    result should be(expected)
  }
}
