package lib

import scala.util.{Failure, Success, Try}
import org.scalatest.{FunSpec, Matchers}

class TestHelperSpec extends FunSpec with Matchers {

  it("All services are valid") {
    val errors = Seq(
      "apidoc-api.json",
      "apidoc-generator.json",
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

}
