package scala.generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import scala.models.Play23ClientGenerator
import scala.models.ning.Ning18ClientGenerator

import scala.models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class ReferenceWithImportsSpec extends FunSpec with Matchers {

  lazy val ssd = new ScalaService(TestHelper.referenceWithImportsApiService)

  it("user case classes") {
    val model = ssd.models.find(_.name == "User").get
    val code = ScalaCaseClasses.generateCaseClass(model, Seq.empty)
    TestHelper.assertEqualsFile("/generators/reference-spec-user-case-class.txt", code)
  }

  it("member case classes") {
    val model = ssd.models.find(_.name == "Member").get
    val code = ScalaCaseClasses.generateCaseClass(model, Seq.empty)
    TestHelper.assertEqualsFile("/generators/reference-spec-member-case-class.txt", code)
  }

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = TestHelper.referenceWithImportsApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        TestHelper.assertEqualsFile("/generators/reference-with-imports-spec-play-23.txt", code)
      }
    }
  }

  it("generates expected code for ning client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = TestHelper.referenceWithImportsApiService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        println(code)
        TestHelper.assertEqualsFile("/generators/reference-with-imports-spec-ning-client.txt", code)
      }
    }
  }

}


