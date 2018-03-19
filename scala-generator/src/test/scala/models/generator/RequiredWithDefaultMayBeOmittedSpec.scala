package models.generator

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

import scala.generator.ScalaService

class RequiredWithDefaultMayBeOmittedSpec extends FunSpec with Matchers {

  val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "unions": [],
      "resources": [],

      "models": []
  """)

  it("allows a required, defaulted field (attributes) to be omitted") {
    val service = TestHelper.service(json)
    service.attributes should be(Nil)
  }

}


