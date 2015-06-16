package scala.generator

import com.bryzek.apidoc.spec.v0.models.Method
import org.scalatest.{ShouldMatchers, FunSpec}

class NamespacesSpec extends FunSpec with ShouldMatchers {

  it("quotes keywords") {
    val ns = Namespaces("com.bryzek.apidoc.example.union.type.v0")
    ns.base should be("com.bryzek.apidoc.example.union.`type`.v0")
  }

}
