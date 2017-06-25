package lib

import scala.util.{Failure, Success, Try}
import org.scalatest.{FunSpec, Matchers}
import models.TestHelper
import org.scalatest.exceptions.TestFailedException

class TestHelperSpec extends FunSpec with Matchers {

  it("All services are valid") {
    val errors = Seq(
      "apidoc-api.json",
      "apidoc-generator.json",
      "collection-json-defaults.json",
      "reference-service.json",
      "reference-with-imports.json",
      "response-with-unit-type.json"
    ).flatMap { file =>
      Try(
        models.TestHelper.parseFile(s"/examples/$file")
      ) match {
        case Success(_) => None
        case Failure(ex) => Some(s"$file: $ex")
      }
    }

    errors should be(Nil)
  }

  describe("assertValidScalaSourceCode") {
    it("accepts valid Scala source code") {
      val code = """
          |import java.util.Calendar
          |import scala.concurrent.Future
          |
          |trait MyTrait
          |
          |class Foobar extends MyTrait
          |
        """.stripMargin
      TestHelper.assertValidScalaSourceCode(code)
    }

    it("throws exception when source code is invalid") {
      intercept[TestFailedException] { TestHelper.assertValidScalaSourceCode("invalid Scala code") }
    }
  }
}
