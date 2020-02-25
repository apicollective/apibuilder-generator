package utils

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScalaFormatterSpec extends AnyFunSpec with Matchers {

  it("ScalaFormatter should format valid scala code") {
    val contents = "case class Foo(bar: String)"
    val result = ScalaFormatter.format(contents)

    result should be(Symbol("right"))
  }

  it("ScalaFormatter should fail to format invalid scala code") {
    val contents = "Foo Bar"
    val result = ScalaFormatter.format(contents)

    result should be(Symbol("left"))
  }

}
