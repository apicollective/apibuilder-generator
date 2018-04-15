package scala.models

import org.scalatest.{FunSpec, Matchers}

class JsonImportsSpec extends FunSpec with Matchers {

  it("basic service") {
    JsonImports(models.TestHelper.referenceApiService) should be(
      Seq(
        "import io.apibuilder.reference.api.v0.models.json._"
      )
    )
  }

  it("includes imports") {
    JsonImports(models.TestHelper.generatorApiService) should be(
      Seq(
        "import io.apibuilder.common.v0.models.json._",
        "import io.apibuilder.generator.v0.models.json._",
        "import io.apibuilder.spec.v0.models.json._"
      )
    )
  }

}
