package scala.generator

import io.apibuilder.spec.v0.models.Method
import org.scalatest.{ShouldMatchers, FunSpec}

class NamespacesSpec extends FunSpec with ShouldMatchers {

  it("quotes keywords") {
    val ns = Namespaces("io.apibuilder.example.union.type.v0")
    ns.base should be("io.apibuilder.example.union.`type`.v0")
  }

}
