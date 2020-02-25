package scala.models.play

import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.generator.v0.models.gens._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.models.play.Helpers.compareWithoutWhiteSpaces

class Play26GeneratorSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

  implicit val scalacheckConfig = generatorDrivenConfig.copy(sizeRange = 10)

  it("prependHeader should prepend a header") {
    forAll { (header: String, contents: String, form: InvocationForm) =>
      val expected = s"""
          ${header}

          ${contents}
      """

      val result = Play26Generator.prependHeader(contents, form, _ => header)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("file should wrap generator.ServiceFileNames.toFile") {
    forAll { (form: InvocationForm, suffix: String, contents: String, extension: Option[String]) =>
      val expected = generator.ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, suffix, contents, extension)
      val result = Play26Generator.file(form, suffix, contents, extension)

      result should be(expected)
    }
  }

  it("formatRoutes should format routes") {
    forAll { (lines: List[String], spacesCount: Byte) =>
      val spaces = " " * spacesCount
      val contents = lines.mkString(spaces, s"${spaces}\n${spaces}", spaces)
      val expected = lines.mkString("\n").trim
      val result = Play26Generator.formatRoutes(contents)

      result should be(expected)
    }
  }
}
