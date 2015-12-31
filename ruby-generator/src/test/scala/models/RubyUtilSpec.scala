package ruby.models

import java.io.File

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models.{Enum, EnumValue}

import org.scalatest.{ FunSpec, Matchers }

class RubyUtilSpec extends FunSpec with Matchers {

  it("RubyUtil.Module") {
    RubyUtil.Module("com.bryzek.apidoc.v0").parts should be(Seq("Com", "Bryzek", "Apidoc", "V0"))
    RubyUtil.Module("com.bryzek.apidoc.v0").fullName should be("::Com::Bryzek::Apidoc::V0")
    RubyUtil.Module("foobar").fullName should be("::Foobar")
    RubyUtil.Module("foo_bar").fullName should be("::FooBar")
  }

  it("RubyUtil.toVariable") {
    RubyUtil.toVariable("value", multiple = false) should be("value")
    RubyUtil.toVariable("value", multiple = true) should be("values")

    RubyUtil.toVariable("org_key", multiple = false) should be("org_key")
    RubyUtil.toVariable("org_key", multiple = true) should be("org_keys")

    RubyUtil.toVariable("orgKey", multiple = false) should be("org_key")
    RubyUtil.toVariable("orgKey", multiple = true) should be("org_keys")

    RubyUtil.toVariable("com.bryzek.foo", multiple = false) should be("com_bryzek_foo")
    RubyUtil.toVariable("com.bryzek.foo", multiple = true) should be("com_bryzek_foos")

    RubyUtil.toVariable("class", multiple = false) should be("class_")
    RubyUtil.toVariable("class", multiple = true) should be("classes")
  }

  it("RubyUtil.wrapInQuotes") {
    RubyUtil.wrapInQuotes("value") should be("'value'")
    RubyUtil.wrapInQuotes("'value'") should be(""""'value'"""")
  }

  it("RubyUtil.toConstant") {
    RubyUtil.toConstant("foo") should be("FOO")
    RubyUtil.toConstant("fooBar") should be("FOO_BAR")
    RubyUtil.toConstant("foo_bar") should be("FOO_BAR")
    RubyUtil.toConstant("FOO_BAR") should be("FOO_BAR")
  }

  it("RubyUtil.toMethodName") {
    RubyUtil.toMethodName("foo") should be("foo")
    RubyUtil.toMethodName("foo_bar") should be("foo_bar")
    RubyUtil.toMethodName("foo.bar") should be("foo_bar")
    RubyUtil.toMethodName("fooBar") should be("foo_bar")
  }

  describe("rubyDefault") {
    import lib.Datatype.Container
    import lib.Datatype.Primitive
    import lib.Datatype.UserDefined

    it ("should fail for datatype: object") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", Primitive.Object)
      }
      ex.getMessage should be("parsing default `` for datatype Object")
    }

    it ("should fail for datatype: unit") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", Primitive.Unit)
      }
      ex.getMessage should be("parsing default `` for datatype Unit")
    }

    it ("default: foo, datatype: enum") {
      RubyUtil.rubyDefault("foo", UserDefined.Enum("test")) should be("""Test.apply("foo")""")
    }

    it ("should fail for datatype: model") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", UserDefined.Model("test"))
      }
      ex.getMessage should be("parsing default `` for datatype Model(test)")
    }

    it ("should fail for datatype: union") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", UserDefined.Union("test"))
      }
      ex.getMessage should be("parsing default `` for datatype Union(test)")
    }

    it ("default: true, datatype: boolean") {
      RubyUtil.rubyDefault("true", Primitive.Boolean) should be("true")
    }

    it ("default: false, datatype: boolean") {
      RubyUtil.rubyDefault("false", Primitive.Boolean) should be("false")
    }

    it ("default: 1234.5, datatype: double") {
      RubyUtil.rubyDefault("1234.5", Primitive.Double) should be("1234.5")
    }

    it ("default: -1234.5, datatype: double") {
      RubyUtil.rubyDefault("-1234.5", Primitive.Double) should be("-1234.5")
    }

    it ("default: 123, datatype: double") {
      RubyUtil.rubyDefault("123", Primitive.Double) should be("123.0")
    }

    it ("default: 124, datatype: integer") {
      RubyUtil.rubyDefault("124", Primitive.Integer) should be("124")
    }

    it ("default: -124, datatype: integer") {
      RubyUtil.rubyDefault("-124", Primitive.Integer) should be("-124")
    }

    it ("default: 124, datatype: long") {
      RubyUtil.rubyDefault("124", Primitive.Long) should be("124")
    }

    it ("default: -124, datatype: long") {
      RubyUtil.rubyDefault("-124", Primitive.Long) should be("-124")
    }

    it ("default: 2014-03-14, datatype: date-iso8601") {
      RubyUtil.rubyDefault("2014-03-14", Primitive.DateIso8601) should be("""Date.parse("2014-03-14")""")
    }

    it ("default: 2014-03-14T12:13:15Z, datatype: date-time-iso8601") {
      RubyUtil.rubyDefault("2014-03-14T12:13:15Z", Primitive.DateTimeIso8601) should be {
        """DateTime.parse("2014-03-14T12:13:15Z")"""
      }
    }

    it ("default: 123453.8, datatype: decimal") {
      RubyUtil.rubyDefault("123453.8", Primitive.Decimal) should be("123453.8")
    }

    it ("default: foo, datatype: string") {
      RubyUtil.rubyDefault("foo", Primitive.String) should be(""""foo"""")
    }

    it ("default: 956169ba-d25e-11e4-8cf3-2bf83a2580ee, datatype: uuid") {
      RubyUtil.rubyDefault("956169ba-d25e-11e4-8cf3-2bf83a2580ee", Primitive.Uuid) should be("""UUID.new("956169ba-d25e-11e4-8cf3-2bf83a2580ee")""")
    }

    it ("""default: ["foo"], datatype: [string]""") {
      RubyUtil.rubyDefault("""["foo"]""", Container.List(Primitive.String)) should be("""["foo"]""")
    }

    it ("default: [], datatype: [string]") {
      RubyUtil.rubyDefault("""[]""", Container.List(Primitive.String)) should be("""[]""")
    }

    it ("""default: {"name": "foo"}, datatype: map[integer]""") {
      RubyUtil.rubyDefault("""{"name": "foo"}""", Container.Map(Primitive.String)) should be("""{"name" => "foo"}""")
    }

    it ("empty map, datatype: map[integer]") {
      RubyUtil.rubyDefault("""{}""", Container.Map(Primitive.String)) should be("""{}""")
    }
  }
}
