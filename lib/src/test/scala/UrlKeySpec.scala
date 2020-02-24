package lib

import org.scalatest.flatspec.AnyFlatSpec

class UrlKeySpec extends AnyFlatSpec {

  behavior of "Url Key Generator"

  it should "leave good urls alone" in {
    assert("foo" === UrlKey.generate("foo"))
    assert("foo-bar" === UrlKey.generate("foo-bar"))
  }

  it should "leave numbers alone" in {
    assert("foo123" === UrlKey.generate("foo123"))
  }

  it should "lower case" in {
    assert("foo-bar" === UrlKey.generate("FOO-BAR"))
  }

  it should "trim" in {
    assert("foo-bar" === UrlKey.generate("  FOO-BAR  "))
  }

}
