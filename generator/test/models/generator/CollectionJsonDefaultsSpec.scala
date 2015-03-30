package generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import models.{Play23ClientGenerator, RubyClientGenerator}
import models.ning.Ning18ClientGenerator

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class CollectionJsonDefaultsSpec extends FunSpec with Matchers {

  lazy val ssd = new ScalaService(TestHelper.collectionJsonDefaultsService)

  it("user case classes") {
    val model = ssd.models.find(_.name == "User").get
    val code = ScalaCaseClasses.generateCaseClass(model, Seq.empty)
    TestHelper.assertEqualsFile("test/resources/generators/collection-json-defaults-user-case-class.txt", code)
  }

  it("user_patch case classes") {
    val model = ssd.models.find(_.name == "UserPatch").get
    val code = ScalaCaseClasses.generateCaseClass(model, Seq.empty)
    TestHelper.assertEqualsFile("test/resources/generators/collection-json-defaults-user-patch-case-class.txt", code)
  }

  it("generates expected code for play 2.3 client") {
    val code = Play23ClientGenerator.invoke(InvocationForm(service = TestHelper.collectionJsonDefaultsService))
    TestHelper.assertEqualsFile("test/resources/generators/collection-json-defaults-play-23.txt", code)
  }

  it("generates expected code for ning client") {
    val code = Ning18ClientGenerator.invoke(InvocationForm(service = TestHelper.collectionJsonDefaultsService))
    TestHelper.assertEqualsFile("test/resources/generators/collection-json-defaults-ning-client.txt", code)
  }

  it("generates expected code for ruby client") {
    val code = RubyClientGenerator.invoke(InvocationForm(service = TestHelper.collectionJsonDefaultsService))
    TestHelper.assertEqualsFile("test/resources/generators/collection-json-defaults-ruby-client.txt", code)
  }

}


