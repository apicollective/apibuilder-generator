package scala.models

import scala.generator.ScalaService
import org.scalatest.{ ShouldMatchers, FunSpec }

class JsonImportsSpec extends FunSpec with ShouldMatchers {

  it("basic service") {
    JsonImports(models.TestHelper.referenceApiService) should be(
      Seq(
        "import com.bryzek.apidoc.reference.api.v0.models.json._"
      )
    )
  }

  it("includes imports") {
    println(JsonImports(models.TestHelper.generatorApiService))
    JsonImports(models.TestHelper.generatorApiService) should be(
      Seq(
        "import com.bryzek.apidoc.common.v0.models.json._",
        "import com.bryzek.apidoc.generator.v0.models.json._",
        "import com.bryzek.apidoc.spec.v0.models.json._"
      )
    )
  }

}
