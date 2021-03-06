package scala.generator

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NamespacesSpec extends AnyFunSpec with Matchers {

  it("quotes keywords") {
    val ns = Namespaces("io.apibuilder.example.union.type.v0")
    ns.base should be("io.apibuilder.example.union.`type`.v0")
    ns.enums should be ("io.apibuilder.example.union.`type`.v0.models")
    ns.models should be ("io.apibuilder.example.union.`type`.v0.models")
    ns.unions should be ("io.apibuilder.example.union.`type`.v0.models")
    ns.mock should be ("io.apibuilder.example.union.`type`.v0.mock")
  }

}
