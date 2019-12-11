package models.play

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import models.TestHelper.{assertJodaTimeNotPresent, assertValidScalaSourceCode}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scala.models.{Play26EnvelopeClientGenerator, Play27ClientGenerator}

class Play26EnvelopeGeneratorSpec extends FunSpec with Matchers with PropertyChecks {

  it("generates response envelope") {
    val form = InvocationForm(
      models.TestHelper.dateTimeService,
      Seq(Attribute("scala_generator.response", "envelope")),
      None
    )
    val Right(files) = Play26EnvelopeClientGenerator.invoke(form)
    assertValidScalaSourceCode(files(0).contents)
    models.TestHelper.assertEqualsFile(s"/generators/play-26-envelope.txt", files(0).contents)
  }

}
