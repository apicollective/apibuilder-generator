package models

import com.gilt.apidoc.spec.v0.models.Service
import generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class Play2BindablesSpec extends FunSpec with ShouldMatchers {

  lazy val service = TestHelper.referenceApiService
  lazy val ssd = new ScalaService(service)

  it("generates bindable for a single enum") {
    TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-age-group.txt",
      Play2Bindables(ssd).buildImplicit("AgeGroup")
    )
  }

  it("generates bindable object") {
    TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-reference-api-object.txt",
      Play2Bindables(ssd).build()
    )
  }

}
