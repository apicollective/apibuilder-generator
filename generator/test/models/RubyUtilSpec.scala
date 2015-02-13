package models

import java.io.File
import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.{Enum, EnumValue}

import org.scalatest.{ FunSpec, Matchers }

class RubyUtilSpec extends FunSpec with Matchers {

  it("RubyUtil.Module") {
    RubyUtil.Module("com.gilt.apidoc.v0").parts should be(Seq("Com", "Gilt", "Apidoc", "V0"))
    RubyUtil.Module("com.gilt.apidoc.v0").fullName should be("Com::Gilt::Apidoc::V0")
    RubyUtil.Module("foobar").fullName should be("Foobar")
    RubyUtil.Module("foo_bar").fullName should be("FooBar")
  }

  it("RubyUtil.toVariableName") {
    RubyUtil.toVariable("value", multiple = false) should be("value")
    RubyUtil.toVariable("value", multiple = true) should be("values")

    RubyUtil.toVariable("org_key", multiple = false) should be("org_key")
    RubyUtil.toVariable("org_key", multiple = true) should be("org_keys")

    RubyUtil.toVariable("orgKey", multiple = false) should be("org_key")
    RubyUtil.toVariable("orgKey", multiple = true) should be("org_keys")
  }

  it("RubyUtil.wrapInQuotes") {
    RubyUtil.wrapInQuotes("value") should be("'value'")
    RubyUtil.wrapInQuotes("'value'") should be(""""'value'"""")
  }

}
