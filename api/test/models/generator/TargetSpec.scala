package generator

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class TargetSpec extends FunSpec with Matchers {

  private val Path = "api.json"
  private lazy val service = TestHelper.parseFile(Path)

  it("Has a field named target") {
    service.models("generator").fields.find(_.name == "key").getOrElse {
      sys.error("Cannot find generator.key field")
    }
  }

}
