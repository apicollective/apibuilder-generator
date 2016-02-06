package go.models

import org.scalatest.{FunSpec, Matchers}
import Formatter._

class FormatterSpec extends FunSpec with Matchers {

  it("table") {
    "a".table() should be("a")
    "a b".table() should be("a b")
    "a b\nc   d".table() should be("a b\nc d")
    "a b\nfoo\nc   d".table() should be("a b\nfoo\nc d")
  }

  it("table preserves leading spaces") {
    "  a".table() should be("  a")
  }

  it("indent") {
    "foo"indent(0) should be("foo")
    "foo"indent(1) should be("\tfoo")
    "foo"indent(2) should be("\t\tfoo")
  }

}
