package models.http4s

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import models.TestHelper.{assertJodaTimeNotPresent, assertValidScalaSourceCode}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.models.http4s._

class Http4sGeneratorSpec extends AnyFunSpec with Matchers {
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
      val form = InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s015Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/015/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.17") {
      val form = InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s017Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/017/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.18") {
      val form = InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s018Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/018/${file.name}.txt", file.contents)
      }
    }

    it("http4s 0.20") {
      val form = InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
      val Right(files) = Http4s020Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        file.name shouldBe fileNames(idx)
        assertValidScalaSourceCode(file.contents)
        models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/020_instant/${file.name}.txt", file.contents)
      }
    }
      it("http4s 0.22") {
        val form = InvocationForm(models.TestHelper.apidocApiService, Seq.empty, None)
        val Right(files) = Http4s022Generator.invoke(form)
        files.size shouldBe 7
        files.zipWithIndex.foreach { case (file, idx) =>
          file.name shouldBe fileNames(idx)
          assertValidScalaSourceCode(file.contents)
          models.TestHelper.assertEqualsFile(s"/http4s/apidoc-api/022/${file.name}.txt", file.contents)
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

    it("http4s 0.22 java instant") {
      val form = InvocationForm(
        models.TestHelper.dateTimeService,
        Seq.empty,
        None
      )
      val Right(files) = Http4s022Generator.invoke(form)
      files.size shouldBe 7
      assertJodaTimeNotPresent(files)
      files.zipWithIndex.foreach { case (file, idx) =>
        fileNames(idx).foreach { fileName =>
          file.name shouldBe fileName
          assertValidScalaSourceCode(file.contents)
          models.TestHelper.assertEqualsFile(s"/http4s/date-time/022_instant/${file.name}.txt", file.contents)
        }
      }
    }

    it("http4s 0.22 java offsetdatetime") {
      val form = InvocationForm(
        models.TestHelper.dateTimeService,
        Seq(Attribute("scala_generator.date_time.type", "java.offsetdatetime"), Attribute("scala_generator.date.type", "java.localdate")),
        None
      )
      val Right(files) = Http4s022Generator.invoke(form)
      files.size shouldBe 7
      assertJodaTimeNotPresent(files)
      files.zipWithIndex.foreach { case (file, idx) =>
        fileNames(idx).foreach { fileName =>
          file.name shouldBe fileName
          assertValidScalaSourceCode(file.contents)
          models.TestHelper.assertEqualsFile(s"/http4s/date-time/022_offsetdatetime/${file.name}.txt", file.contents)
        }
      }
    }

    it("http4s 0.22 joda datetime") {
      val form = InvocationForm(
        models.TestHelper.dateTimeService,
        Seq(Attribute("scala_generator.time_library", "joda")),
        None
      )
      val Right(files) = Http4s022Generator.invoke(form)
      files.size shouldBe 7
      files.zipWithIndex.foreach { case (file, idx) =>
        fileNames(idx).foreach { fileName =>
          file.name shouldBe fileName
          assertValidScalaSourceCode(file.contents)
          models.TestHelper.assertEqualsFile(s"/http4s/date-time/022_joda/${file.name}.txt", file.contents)
        }
      }
    }
  }
}
