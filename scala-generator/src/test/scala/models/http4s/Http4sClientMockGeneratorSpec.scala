package models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.{FunSpec, Matchers}

import scala.generator.ScalaClientMethodConfigs
import scala.models.http4s.{Http4s017Generator, Http4s018Generator, Http4s019Generator, ScalaService}

class Http4sClientMockGeneratorSpec extends FunSpec with Matchers {

  describe("generate mock") {
    val service = models.TestHelper.generatorApiService
    val ssd = new ScalaService(models.TestHelper.generatorApiService)
    val invocationForm = new InvocationForm(service, Seq.empty, None)

    it("Http4s 0.19 mock generator produces valid Scala source code") {
      val config = new ScalaClientMethodConfigs.Http4s019(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s019Generator.generateMockClientCode(invocationForm, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      sourceCode should include("Applicative[F]")
      sourceCode should include("F[_]")
      sourceCode should include("Applicative[F].pure")
    }

    it("Http4s 0.18 mock generator produces valid Scala source code") {
      val config = new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s018Generator.generateMockClientCode(invocationForm, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      sourceCode should include("Applicative[F]")
      sourceCode should include("F[_]")
      sourceCode should include("Applicative[F].pure")
    }

    it("Http4s 0.17 mock generator produces valid Scala source code") {
      val config = new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None)

      val sourceCode = Http4s017Generator.generateMockClientCode(invocationForm, ssd, config)

      assertValidScalaSourceCode(sourceCode)
      sourceCode should not include ("Applicative[F]")
      sourceCode should include("Task.now")

    }
  }

}
