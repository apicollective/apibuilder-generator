package utils

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScalaFormatterSpec extends AnyFunSpec with Matchers {

  private[this] def format(code: String) = {
    ScalaFormatter.format(code)
  }

  it("ScalaFormatter should format valid scala code") {
    format("case class Foo(bar: String)") should be(Symbol("right"))
  }

  it("ScalaFormatter should fail to format invalid scala code") {
    format("Foo Bar {") should be(Symbol("left"))
  }

}
