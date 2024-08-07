package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Play2StandaloneModelsJsonSpec extends AnyFunSpec with Matchers {

  it("quality") {
    val quality = models.TestHelper.parseFile("/examples/quality.json")

    Play2Scala2StandaloneModelsJson.invoke(InvocationForm(quality)) match {
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

