package generator

import ruby.models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class TargetSpec extends FunSpec with Matchers {

  private lazy val service = TestHelper.generatorApiService

  it("Has a field named target") {
    service.models.find(_.name == "generator").get.fields.find(_.name == "key").getOrElse {
      sys.error("Cannot find generator.key field")
    }
  }

}
