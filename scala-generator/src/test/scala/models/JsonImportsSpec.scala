package scala.models

import scala.generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class JsonImportsSpec extends FunSpec with ShouldMatchers {

  it("basic service") {
    JsonImports(TestHelper.referenceApiService) should be(
      Seq(
        "import com.gilt.apidoc.reference.api.v0.models.json._"
      )
    )
  }

  it("includes imports") {
    JsonImports(TestHelper.generatorApiService) should be(
      Seq(
        "import com.gilt.apidoc.generator.v0.models.json._",
        "import com.gilt.apidoc.spec.v0.models.json._"
      )
    )
  }

}
