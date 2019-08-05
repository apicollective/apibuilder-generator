package models.generator.kotlin

import models.TestHelper.referenceApiService
import org.scalatest.{FunSpec, Matchers}

class NamespacesSpec extends FunSpec
  with Matchers {

  describe("Namespaces") {
    it(s"models, enums, unions") {
      val service = referenceApiService
      service.namespace shouldBe "io.apibuilder.reference.api.v0"
      val namespaces = Namespaces(service.namespace)
      namespaces.models shouldBe "io.apibuilder.reference.api.v0.models"
      namespaces.enums shouldBe "io.apibuilder.reference.api.v0.models"
      namespaces.unions shouldBe "io.apibuilder.reference.api.v0.models"
    }
  }
}
