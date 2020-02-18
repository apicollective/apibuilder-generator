package models.play

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

import scala.models.Play26EnvelopeClientGenerator

class Play26EnvelopeGeneratorSpec extends AnyFunSpec with Matchers {

  it("generates response envelope") {
    val form = InvocationForm(
      models.TestHelper.dateTimeService,
      Seq(Attribute("response", "envelope")),
      None
    )
    val Right(files) = Play26EnvelopeClientGenerator.invoke(form)
    assertValidScalaSourceCode(files(0).contents)
    models.TestHelper.assertEqualsFile(s"/generators/play-26-envelope.txt", files(0).contents)
  }

}
