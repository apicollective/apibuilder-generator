package models.generator.flow

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{Matchers, WordSpec}

import scala.generator.flow.FlowMockClientGenerator

class FlowTestClientPlay26GeneratorSpec extends WordSpec with Matchers {

  "generate flow test client" in {
    FlowMockClientGenerator.Play26.invoke(InvocationForm(service = models.TestHelper.referenceApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/generators/reference-spec-play-26-flow-mock-client.txt", sourceFiles.head.contents)
      }
    }
  }

}
