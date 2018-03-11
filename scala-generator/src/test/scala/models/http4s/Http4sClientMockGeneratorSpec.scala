package models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.{FunSpec, Matchers}

import scala.generator.mock.MockClientGenerator

class Http4sClientMockGeneratorSpec extends FunSpec with Matchers {

  describe("generate mock") {
    it("Http4s 0.18 mock generator produces valid Scala source code") {
      val service = models.TestHelper.generatorApiService
      val invocationForm = new InvocationForm(service, Seq.empty, None)

      MockClientGenerator.Http4s018.invoke(invocationForm) match {
        case Right(generatedFiles) =>
          generatedFiles.size shouldBe 1
          val sourceCode = generatedFiles(0).contents
          assertValidScalaSourceCode(sourceCode)
          sourceCode should include ("Applicative[F]")
          sourceCode should include ("F[_]")
          sourceCode should include ("Applicative[F].pure")
        case Left(errors) =>
          fail(errors.mkString("\n"))
      }
    }
  }

  it("Http4s 0.17 mock generator produces valid Scala source code") {
    val service = models.TestHelper.generatorApiService
    val invocationForm = new InvocationForm(service, Seq.empty, None)

    MockClientGenerator.Http4s017.invoke(invocationForm) match {
      case Right(generatedFiles) =>
        generatedFiles.size shouldBe 1
        val sourceCode = generatedFiles(0).contents
        assertValidScalaSourceCode(sourceCode)
        println(sourceCode)
        sourceCode should not include ("Applicative[F]")
        sourceCode should include("Task.now")
      case Left(errors) =>
        fail(errors.mkString("\n"))
    }
  }

}
