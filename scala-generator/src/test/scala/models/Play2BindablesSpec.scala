package scala.models

import io.apibuilder.generator.v0.models.Attribute

import scala.generator.ScalaService
import org.scalatest.{FunSpec, Matchers}

class Play2BindablesSpec extends FunSpec with Matchers {

  lazy val service = models.TestHelper.referenceApiService
  lazy val ssd = new ScalaService(service)

  it("generates bindable for a single enum") {
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-age-group.txt",
      Play2Bindables(ssd).buildImplicit("AgeGroup")
    )
  }

  it("generates bindable object") {
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-reference-api-object.txt",
      Play2Bindables(ssd).build()
    )
  }

  it("generates bindable for joda time") {
    val service = models.TestHelper.dateTimeService
    val ssd = new ScalaService(service, Attributes(Nil, Attributes.PlayDefaultConfig))
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-joda-date-time.txt",
      Play2Bindables(ssd).build()
    )
  }

  it("generates bindable for java time Instant") {
    val service = models.TestHelper.dateTimeService
    val ssd = new ScalaService(service, Attributes(Seq(Attribute("scala_generator.date_time.type", "java.instant"), Attribute("scala_generator.date.type", "java.localdate")), Attributes.PlayDefaultConfig))
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-java-instant.txt",
      Play2Bindables(ssd).build()
    )
  }

  it("generates bindable for java time OffsetDateTime") {
    val service = models.TestHelper.dateTimeService
    val ssd = new ScalaService(service, Attributes(Seq(Attribute("scala_generator.date_time.type", "java.offsetdatetime"), Attribute("scala_generator.date.type", "java.localdate")), Attributes.PlayDefaultConfig))
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-java-offsetdatetime.txt",
      Play2Bindables(ssd).build()
    )
  }
}
