package scala.models

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, ShouldMatchers}

class Play2StandaloneModelsJsonSpec extends FunSpec with ShouldMatchers {

  it("quality") {
    val quality = models.TestHelper.parseFile("/examples/quality.json")

    Play2StandaloneModelsJson.invoke(InvocationForm(quality)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(files) => {
        files.map(_.name) should be (Seq("GiltQualityV0Models.scala"))
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-standalone-json-spec-quality.txt",
          files.head.contents
        )
      }
    }
  }

}

