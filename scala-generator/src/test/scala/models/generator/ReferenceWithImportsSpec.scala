package scala.generator

import io.apibuilder.generator.v0.models.InvocationForm
import scala.models.Play23ClientGenerator
import scala.models.ning.Ning18ClientGenerator

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class ReferenceWithImportsSpec extends FunSpec with Matchers {

  lazy val ssd = new ScalaService(models.TestHelper.referenceWithImportsApiService)

  it("user case classes") {
    val model = ssd.models.find(_.name == "User").get
    val code = ScalaCaseClasses.generateCaseClassWithDoc(model, Seq.empty)
    models.TestHelper.assertEqualsFile("/generators/reference-spec-user-case-class", code)
  }

  it("member case classes") {
    val model = ssd.models.find(_.name == "Member").get
    val code = ScalaCaseClasses.generateCaseClassWithDoc(model, Seq.empty)
    models.TestHelper.assertEqualsFile("/generators/reference-spec-member-case-class", code)
  }

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = models.TestHelper.referenceWithImportsApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        val sourceCode = sourceFiles.head.contents
        TestHelper.assertValidScalaSourceCode(sourceCode)
        TestHelper.assertEqualsFile("/generators/reference-with-imports-spec-play-23", sourceCode)
      }
    }
  }

  it("generates expected code for ning client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = models.TestHelper.referenceWithImportsApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        val sourceCode = sourceFiles.head.contents
        TestHelper.assertValidScalaSourceCode(sourceCode)
        TestHelper.assertEqualsFile("/generators/reference-with-imports-spec-ning-client", sourceCode)
      }
    }
  }

}


