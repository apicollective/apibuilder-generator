package scala.models

import io.apibuilder.generator.v0.models.Attribute
import io.apibuilder.spec.v0.models.Service

import scala.generator.ScalaService
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Play2BindablesSpec extends AnyFunSpec with Matchers {

  private lazy val service: Service = models.TestHelper.referenceApiService
  private lazy val ssd: ScalaService = ScalaService(service)

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
    val ssd = ScalaService(service)
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-joda-date-time.txt",
      Play2Bindables(ssd).build()
    )
  }

  it("generates bindable for java time Instant") {
    val service = models.TestHelper.dateTimeService
    val ssd = new ScalaService(service, Attributes.PlayDefaultConfig.withAttributes(Seq(Attribute("scala_generator.date_time.type", "java.instant"), Attribute("scala_generator.date.type", "java.localdate"))), Nil)
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-java-instant.txt",
      Play2Bindables(ssd).build()
    )
  }

  it("generates bindable for java time OffsetDateTime") {
    val service = models.TestHelper.dateTimeService
    val ssd = new ScalaService(service, Attributes.PlayDefaultConfig.withAttributes(Seq(Attribute("scala_generator.date_time.type", "java.offsetdatetime"), Attribute("scala_generator.date.type", "java.localdate"))), Nil)
    models.TestHelper.assertEqualsFile(
      "/generators/play-2-bindable-java-offsetdatetime.txt",
      Play2Bindables(ssd).build()
    )
  }
}
