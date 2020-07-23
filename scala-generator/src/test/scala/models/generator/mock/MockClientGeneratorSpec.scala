package scala.generator.mock

import models.TestHelper.assertValidScalaSourceCode

import scala.generator.{ScalaClientMethodConfigs, ScalaService}
import scala.generator.mock.MockClientGenerator._
import scala.generator.ScalaField.Limitation
import scala.models.{Attributes, DateTimeTypeConfig, DateTypeConfig, ResponseConfig}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MockClientGeneratorSpec extends AnyFunSpec with Matchers {
  it("should generate the right desired length for string given a field limitation") {
    calculateStringLength(Limitation(None, None)) should be(24)
    calculateStringLength(Limitation(Some(6), None)) should be(24) // TODO or [6, 24] ??? -> mostly for very small min vals e.g. min=3
    calculateStringLength(Limitation(Some(25), None)) should be(25)
    calculateStringLength(Limitation(None, Some(23))) should be(23)
    // TODO or [24, 10000] ??? hmm, rather not ==> leave 24
    //   this is not symmetric problem -- as min has a natural lower bound of 0, while max has a very high upper bound
    calculateStringLength(Limitation(None, Some(10000))) should be(24)

    val lenFrom22And25 = calculateStringLength(Limitation(Some(22), Some(25)))
    lenFrom22And25 should be >= 22
    lenFrom22And25 should be <= 25

    val lenFrom6And8 = calculateStringLength(Limitation(Some(6), Some(8)))
    lenFrom6And8 should be >= 6
    lenFrom6And8 should be <= 8

    calculateStringLength(Limitation(Some(3), Some(3))) should be(3)

    calculateStringLength(Limitation(Some(25), Some(22))) should be(0)
    calculateStringLength(Limitation(Some(22), Some(25))) should be(23)
    calculateStringLength(Limitation(None, Some(-1))) should be(0)

    calculateStringLength(Limitation(Some(Int.MaxValue), None)) should be(Int.MaxValue) // TODO: really?!
    calculateStringLength(Limitation(Some(Long.MaxValue), None)) should be(Int.MaxValue) // TODO: really?!
  }

  describe("Play 2.7 - date and date-time types") {
    it("uses joda time") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JodaLocalDate, dateTimeType = DateTimeTypeConfig.JodaDateTime))

      val config = new ScalaClientMethodConfigs.Play27(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play27_date-time-joda.txt", sourceCode)
    }

    it("uses java time with Instant") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaInstant))

      val config = new ScalaClientMethodConfigs.Play27(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play27_date-time-instant.txt", sourceCode)
    }

    it("uses java time with OffsetDateTime") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaOffsetDateTime))

      val config = new ScalaClientMethodConfigs.Play27(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play27_date-time-offsetdatetime.txt", sourceCode)
    }
  }

  describe("Play 2.8 - date and date-time types") {
    it("uses joda time") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JodaLocalDate, dateTimeType = DateTimeTypeConfig.JodaDateTime))

      val config = new ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_date-time-joda.txt", sourceCode)
    }

    it("uses java time with Instant") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaInstant))

      val config = new ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_date-time-instant.txt", sourceCode)
    }

    it("uses java time with OffsetDateTime") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaOffsetDateTime))

      val config = new ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = Attributes.PlayDefaultConfig, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_date-time-offsetdatetime.txt", sourceCode)
    }
  }

  describe("play 2.8 - response envelope") {
    it("generates mock clients with ResponseImpl for empty bodies") {
      val service = models.TestHelper.statusCodesService
      val conf = Attributes.PlayDefaultConfig.copy(response = ResponseConfig.Envelope)
      val ssd = new ScalaService(service, conf)

      val config = ScalaClientMethodConfigs.Play28(namespace = "whatever", attributes = conf, baseUrl = None)

      val sourceCode = new MockClientGenerator(ssd, None, config).generateCode()

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/play2/mock-client/play28_empty_bodies.txt", sourceCode)
    }
  }

}
