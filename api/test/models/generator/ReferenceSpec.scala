package generator

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class ReferenceSpec extends FunSpec with Matchers {

  lazy val service = TestHelper.parseFile(s"reference-api/api.json")
  lazy val ssd = new ScalaService(service)

  it("user case classes") {
    val model = ssd.models.find(_.name == "user").get
    val code = ScalaCaseClasses.generateCaseClass(model)
    TestHelper.assertEqualsFile("test/resources/generators/reference-spec-user-case-class.txt", code)
  }

  it("member case classes") {
    val model = ssd.models.find(_.name == "member").get
    val code = ScalaCaseClasses.generateCaseClass(model)
    TestHelper.assertEqualsFile("test/resources/generators/reference-spec-member-case-class.txt", code)
  }

}


