package utils

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.generator.v0.models.gens._
import io.apibuilder.spec.v0.models.{Application, Organization, Service}
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.models.play.Helpers.compareWithoutWhiteSpaces

class ScalaFormatterSpec extends FunSpec with Matchers with PropertyChecks {

  it("ScalaFormatter should format valid scala code") {
    val contents = "case class Foo(bar: String)"
    val result = ScalaFormatter.format(contents)

    result should be('right)
  }

  it("ScalaFormatter should fail to format invalid scala code") {
    val contents = "Foo Bar"
    val result = ScalaFormatter.format(contents)

    result should be('left)
  }

}
