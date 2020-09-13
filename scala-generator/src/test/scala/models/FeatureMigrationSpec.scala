package scala.models

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class FeatureMigrationSpec extends AnyFunSpec with Matchers {

  it("hasImplicit404s") {
    def test(value: String) = FeatureMigration(value).hasImplicit404s()
    test("0.0.1") should be(true)
    test("0.9.4") should be(true)
    test("0.9.5") should be(false)
    test("1.0.0") should be(false)
  }

}
