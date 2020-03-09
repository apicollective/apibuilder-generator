package scala.generator

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TargetSpec extends AnyFunSpec with Matchers {

  private lazy val service = models.TestHelper.generatorApiService

  it("Has a field named target") {
    service.models.find(_.name == "generator").get.fields.find(_.name == "key").getOrElse {
      sys.error("Cannot find generator.key field")
    }
  }

}
