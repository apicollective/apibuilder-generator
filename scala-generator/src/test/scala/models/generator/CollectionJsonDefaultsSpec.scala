package scala.generator

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.models.Play23ClientGenerator
import scala.models.ning.{AsyncHttpClientGenerator, Ning18ClientGenerator}

class CollectionJsonDefaultsSpec extends AnyFunSpec with Matchers {

  private lazy val ssd: ScalaService = ScalaService(models.TestHelper.collectionJsonDefaultsService)

  it("user case classes") {
    val model = ssd.models.find(_.name == "User").get
    val code = ScalaCaseClasses.generateCaseClassWithDoc(model, Seq.empty).build
    models.TestHelper.assertEqualsFile("/generators/collection-json-defaults-user-case-class.txt", code)
  }

  it("user_patch case classes") {
    val model = ssd.models.find(_.name == "UserPatch").get
    val code = ScalaCaseClasses.generateCaseClassWithDoc(model, Seq.empty).build
    models.TestHelper.assertEqualsFile("/generators/collection-json-defaults-user-patch-case-class.txt", code)
  }

  it("generates expected code for play 2.3 client") {
    Play23ClientGenerator.invoke(InvocationForm(service = models.TestHelper.collectionJsonDefaultsService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1

        models.TestHelper.assertEqualsFile("/generators/collection-json-defaults-play-23.txt", sourceFiles.head.contents)
      }
    }
  }

  it("generates expected code for ning client") {
    Ning18ClientGenerator.invoke(InvocationForm(service = models.TestHelper.collectionJsonDefaultsService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/generators/collection-json-defaults-ning-client.txt", sourceFiles.head.contents)
      }
    }
  }

  it("generates expected code for async http client") {
    AsyncHttpClientGenerator.invoke(InvocationForm(service = models.TestHelper.collectionJsonDefaultsService)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/generators/collection-json-defaults-ahc-client.txt", sourceFiles.head.contents)
      }
    }
  }
}
