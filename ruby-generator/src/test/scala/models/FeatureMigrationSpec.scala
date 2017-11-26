package ruby.models

import org.scalatest.{Matchers, FunSpec}

class FeatureMigrationSpec extends FunSpec with Matchers {

  it("hasImplicit404s") {
    FeatureMigration("0.0.1").hasImplicit404s should be(true)
    FeatureMigration("0.9.4").hasImplicit404s should be(true)
    FeatureMigration("0.9.5").hasImplicit404s should be(false)
    FeatureMigration("1.0.0").hasImplicit404s should be(false)
  }

}
