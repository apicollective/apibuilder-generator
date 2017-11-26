package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, Matchers}

class Play2StandaloneModelsJsonSpec extends FunSpec with Matchers {

  it("quality") {
    val quality = models.TestHelper.parseFile("/examples/quality.json")

    Play2StandaloneModelsJson.invoke(InvocationForm(quality)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(files) => {
        files.map(_.name) should be (Seq("GiltQualityV0Models.scala"))
        val scalaSourceCode = files.head.contents
        models.TestHelper.assertValidScalaSourceCode(scalaSourceCode)
        models.TestHelper.assertEqualsFile(
          "/generators/play-2-standalone-json-spec-quality.txt",
          scalaSourceCode
        )
      }
    }
  }

}

