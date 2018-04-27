package scala.generator

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Play23ClientGenerator
import scala.models.ning.{Ning18ClientGenerator, Ning19ClientGenerator, AsyncHttpClientGenerator}
import org.scalatest.{FunSpec, Matchers}

class ReferenceSpec extends FunSpec with Matchers {

  lazy val ssd = new ScalaService(models.TestHelper.referenceApiService)

  it("user case classes") {
    val model = ssd.models.find(_.name == "User").get
    val code = ScalaCaseClasses.generateCaseClassWithDoc(model, Seq.empty)
    models.TestHelper.assertEqualsFile("/generators/reference-spec-user-case-class.txt", code)
  }

  it("member case classes") {
    val model = ssd.models.find(_.name == "Member").get
    val code = ScalaCaseClasses.generateCaseClassWithDoc(model, Seq.empty)
    models.TestHelper.assertEqualsFile("/generators/reference-spec-member-case-class.txt", code)
  }

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = models.TestHelper.referenceApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/generators/reference-spec-play-23.txt", sourceFiles.head.contents)
      }
    }
  }

  it("generates expected code for ning 1.8 client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = models.TestHelper.referenceApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertValidScalaSourceFiles(sourceFiles)
        models.TestHelper.assertEqualsFile("/generators/reference-spec-ning-client.txt", sourceFiles.head.contents)
      }
    }
  }

  it("generates expected code for ning 1.9 client") {
    Ning19ClientGenerator.invoke(InvocationForm(service = models.TestHelper.referenceApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertValidScalaSourceFiles(sourceFiles)
      }
    }
  }

  it("generates expected code for async http client") {
    AsyncHttpClientGenerator.invoke(InvocationForm(service = models.TestHelper.referenceApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertValidScalaSourceFiles(sourceFiles)
      }
    }
  }
}
