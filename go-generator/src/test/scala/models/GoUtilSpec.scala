package go.models

import org.scalatest.{ FunSpec, Matchers }

class GoUtilSpec extends FunSpec with Matchers {

  it("quoteNameIfKeyword") {
    GoUtil.quoteNameIfKeyword("foo") should be("foo")
    GoUtil.quoteNameIfKeyword("int") should be("int_")
  }

  it("textToComment") {
    GoUtil.textToComment(None) should be("")
    GoUtil.textToComment(Some("foo")) should be("""
/**
 * foo
 */
""".trim + "\n")
  }

  it("textToSingleLineComment") {
    GoUtil.textToSingleLineComment(None) should be("")
    GoUtil.textToSingleLineComment(Some("foo")) should be("// foo\n")
  }

  it("wrapInQuotes") {
    GoUtil.wrapInQuotes("foo") should be(""""foo"""")
  }

  it("publicName") {
    GoUtil.publicName("foo") should be("Foo")
    GoUtil.publicName("Foo") should be("Foo")
    GoUtil.publicName("fooBar") should be("FooBar")
    GoUtil.publicName("foo_Bar") should be("FooBar")
    GoUtil.publicName("fooBar") should be("FooBar")
    GoUtil.publicName("int") should be("Int_")
  }

  it("privateName") {
    GoUtil.privateName("foo") should be("foo")
    GoUtil.privateName("Foo") should be("foo")
    GoUtil.privateName("fooBar") should be("fooBar")
    GoUtil.privateName("foo_Bar") should be("fooBar")
    GoUtil.privateName("fooBar") should be("fooBar")
    GoUtil.privateName("int") should be("int_")
  }

  it("packageName") {
    GoUtil.packageName("foo") should be("foo")
    GoUtil.packageName("Foo") should be("foo")
    GoUtil.packageName("fooBar") should be("foobar")
    GoUtil.packageName("foo_Bar") should be("foobar")
    GoUtil.packageName("fooBar") should be("foobar")
    GoUtil.packageName("int") should be("int_")
  }

}
