package scala.generator.mock

import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.{ScalaClientMethodConfigs, ScalaService}
import scala.models.{Attributes, DateTimeTypeConfig, DateTypeConfig, ResponseConfig}

class MockClientGeneratorSpec extends AnyFunSpec with Matchers {

  describe("Play 2.7 - date and date-time types") {
    it("uses joda time") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JodaLocalDate, dateTimeType = DateTimeTypeConfig.JodaDateTime), Nil)

      val config = ScalaClientMethodConfigs.Play27(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play27_date-time-joda.txt", sourceCode)
    }

    it("uses java time with Instant") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaInstant), Nil)

      val config = ScalaClientMethodConfigs.Play27(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play27_date-time-instant.txt", sourceCode)
    }

    it("uses java time with OffsetDateTime") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaOffsetDateTime), Nil)

      val config = ScalaClientMethodConfigs.Play27(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play27_date-time-offsetdatetime.txt", sourceCode)
    }
  }

  describe("Play 2.8 - date and date-time types") {
    it("uses joda time") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JodaLocalDate, dateTimeType = DateTimeTypeConfig.JodaDateTime), Nil)

      val config = ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_date-time-joda.txt", sourceCode)
    }

    it("uses java time with Instant") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaInstant), Nil)

      val config = ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_date-time-instant.txt", sourceCode)
    }

    it("uses java time with OffsetDateTime") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaOffsetDateTime), Nil)

      val config = ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_date-time-offsetdatetime.txt", sourceCode)
    }
  }

  describe("play 2.8 - response envelope") {
    it("generates mock clients with ResponseImpl for empty bodies") {
      val service = models.TestHelper.statusCodesService
      val conf = Attributes.PlayDefaultConfig.copy(response = ResponseConfig.Envelope)
      val ssd = new ScalaService(service, conf, Nil)

      val config = ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = conf, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_empty_bodies.txt", sourceCode)
    }
  }

}
