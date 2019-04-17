package models.http4s

import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.{FunSpec, Matchers}

import scala.generator.ScalaClientMethodConfigs
import scala.models.{Config, DateTimeTypeConfig, DateTypeConfig}
import scala.models.http4s.{Http4s017Generator, Http4s018Generator, Http4s020Generator, ScalaService}

class Http4sClientMockGeneratorSpec extends FunSpec with Matchers {

  describe("generate mock") {
    val service = models.TestHelper.generatorApiService
    val ssd = new ScalaService(service)

    it("Http4s 0.20 mock generator produces valid Scala source code") {
      val config = new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s020Generator.generateMockClientCode(None, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      sourceCode should include("Applicative[F]")
      sourceCode should include("F[_]")
      sourceCode should include("Applicative[F].pure")
    }

    it("Http4s 0.18 mock generator produces valid Scala source code") {
      val config = new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s018Generator.generateMockClientCode(None, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      sourceCode should include("Applicative[F]")
      sourceCode should include("F[_]")
      sourceCode should include("Applicative[F].pure")
    }

    it("Http4s 0.17 mock generator produces valid Scala source code") {
      val config = new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s017Generator.generateMockClientCode(None, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      sourceCode should not include ("Applicative[F]")
      sourceCode should include("Task.now")

    }
  }

  describe("date and date-time types") {
    it("uses joda time") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Config.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JodaLocalDate, dateTimeType = DateTimeTypeConfig.JodaDateTime))

      val config = new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s020Generator.generateMockClientCode(None, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/http4s/mock-client/http4s_020_date-time-joda.txt", sourceCode)
    }

    it("uses java time with Instant") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Config.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaInstant))

      val config = new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s020Generator.generateMockClientCode(None, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/http4s/mock-client/http4s_020_date-time-instant.txt", sourceCode)
    }

    it("uses java time with OffsetDateTime") {
      val service = models.TestHelper.dateTimeService
      val ssd = new ScalaService(service, Config.Http4sDefaultConfig.copy(dateType = DateTypeConfig.JavaLocalDate, dateTimeType = DateTimeTypeConfig.JavaOffsetDateTime))

      val config = new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s020Generator.generateMockClientCode(None, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      models.TestHelper.assertEqualsFile(s"/http4s/mock-client/http4s_020_date-time-offsetdatetime.txt", sourceCode)
    }
  }
}
