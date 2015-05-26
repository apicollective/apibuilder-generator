package scala.generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import scala.models.Play23ClientGenerator
import scala.models.ning.Ning18ClientGenerator

import scala.models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class CollectionJsonDefaultsSpec extends FunSpec with Matchers {

  lazy val ssd = new ScalaService(TestHelper.collectionJsonDefaultsService)

  it("user case classes") {
    val model = ssd.models.find(_.name == "User").get
    val code = ScalaCaseClasses.generateCaseClass(model, Seq.empty)
    TestHelper.assertEqualsFile("/generators/collection-json-defaults-user-case-class.txt", code)
  }

  it("user_patch case classes") {
    val model = ssd.models.find(_.name == "UserPatch").get
    val code = ScalaCaseClasses.generateCaseClass(model, Seq.empty)
    TestHelper.assertEqualsFile("/generators/collection-json-defaults-user-patch-case-class.txt", code)
  }

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = TestHelper.collectionJsonDefaultsService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        TestHelper.assertEqualsFile("/generators/collection-json-defaults-play-23.txt", code)
      }
    }
  }

  it("generates expected code for ning client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = TestHelper.collectionJsonDefaultsService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(code) => {
        TestHelper.assertEqualsFile("/generators/collection-json-defaults-ning-client.txt", code)
      }
    }
  }

}


