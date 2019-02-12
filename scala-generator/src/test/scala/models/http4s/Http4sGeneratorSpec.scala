package models.http4s

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
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
      val form = new InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s015Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/015/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.17") {
      val form = new InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s017Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/017/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.18") {
      val form = new InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s018Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/018/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.20") {
      val form = new InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s020Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/020/${file.name}.txt", file.contents)
      }
    }
  }

  describe("date-time-types") {
    val fileNames = Seq(
      Some("ApibuilderTimeTypesV0Client.scala"),
      None,
      None,
      None,
      None,
      None,
      Some("ApibuilderTimeTypesV0Server.scala"),
    )

    it("http4s 0.20") {
      val form = new InvocationForm(
        models.TestHelper.parseFile("/examples/date-time-types.json"),
        Seq.empty,
        None
      )
      val Right(files) = Http4s020Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        fileNames(idx).foreach { fileName =>
          file.name shouldBe fileName
          assertValidScalaSourceCode(file.contents)
          models.TestHelper.assertEqualsFile(s"/http4s/date-time/020/${file.name}.txt", file.contents)
        }
      }
    }

    it("http4s with joda 0.20") {
      val form = new InvocationForm(
        models.TestHelper.parseFile("/examples/date-time-types.json"),
        Seq(Attribute("scala_generator.time_library", "joda")),
        None
      )
      val Right(files) = Http4s020Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        fileNames(idx).foreach { fileName =>
          file.name shouldBe fileName
          assertValidScalaSourceCode(file.contents)
          models.TestHelper.assertEqualsFile(s"/http4s/date-time/020_joda/${file.name}.txt", file.contents)
        }
      }
    }
  }
}
