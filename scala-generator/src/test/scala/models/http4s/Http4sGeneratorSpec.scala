package models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.{FunSpec, Matchers}

import scala.models.http4s.{Http4s015Generator, Http4s017Generator, Http4s018Generator, Http4s020Generator}

class Http4sGeneratorSpec extends FunSpec with Matchers {
  describe("apidoc-api") {
    val fileNames = Seq(
      "BryzekApidocApiV0Client.scala",
      "BryzekApidocApiV0ModelsJson.scala",
      "BryzekApidocApiV0ModelsOnly.scala",
      "BryzekApidocApiV0JsonOnly.scala",
      "BryzekApidocApiV0ClientOnly.scala",
      "BryzekApidocApiV0MockClient.scala",
      "BryzekApidocApiV0Server.scala",
    )

    it("http4s 0.15") {
      val service = models.TestHelper.parseFile(s"/examples/apidoc-api.json")
      val form = new InvocationForm(service, Seq.empty, None)
      val Right(files) = Http4s015Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/015/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.17") {
      val service = models.TestHelper.parseFile(s"/examples/apidoc-api.json")
      val form = new InvocationForm(service, Seq.empty, None)
      val Right(files) = Http4s017Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/017/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.18") {
      val service = models.TestHelper.parseFile(s"/examples/apidoc-api.json")
      val form = new InvocationForm(service, Seq.empty, None)
      val Right(files) = Http4s018Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/018/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.20") {
      val service = models.TestHelper.parseFile(s"/examples/apidoc-api.json")
      val form = new InvocationForm(service, Seq.empty, None)
      val Right(files) = Http4s020Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/020/${file.name}.txt", file.contents)
      }
    }
  }
}
