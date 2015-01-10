package models

import com.gilt.apidocspec.models.Service
import generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class Play2BindablesSpec extends FunSpec with ShouldMatchers {

  lazy val service = TestHelper.referenceApiService
  lazy val ssd = new ScalaService(service)

  it("generates bindable for a single enum") {
    TestHelper.assertEqualsFile(
      "test/resources/generators/play-2-bindable-age-group.txt",
      Play2Bindables.buildImplicit("AgeGroup")
    )
  }

  it("generates bindable object") {
    TestHelper.assertEqualsFile(
      "test/resources/generators/play-2-bindable-reference-api-object.txt",
      Play2Bindables.build(ssd)
    )
  }

}
