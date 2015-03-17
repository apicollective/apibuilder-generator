package models

import java.io.File

import lib.Kind
import lib.Datatype
import lib.Primitives
import lib.Type

import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.{Enum, EnumValue}

import org.scalatest.{ FunSpec, Matchers }

class RubyUtilSpec extends FunSpec with Matchers {

  it("RubyUtil.Module") {
    RubyUtil.Module("com.gilt.apidoc.v0").parts should be(Seq("Com", "Gilt", "Apidoc", "V0"))
    RubyUtil.Module("com.gilt.apidoc.v0").fullName should be("::Com::Gilt::Apidoc::V0")
    RubyUtil.Module("foobar").fullName should be("::Foobar")
    RubyUtil.Module("foo_bar").fullName should be("::FooBar")
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

  it("RubyUtil.toConstant") {
    RubyUtil.toConstant("foo") should be("FOO")
    RubyUtil.toConstant("fooBar") should be("FOO_BAR")
    RubyUtil.toConstant("foo_bar") should be("FOO_BAR")
    RubyUtil.toConstant("FOO_BAR") should be("FOO_BAR")
  }

  describe("rubyDefault") {
    import lib.Datatype.Singleton

    it ("should fail for datatype: object") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", Singleton(Type(Kind.Primitive, Primitives.Object.toString)))
      }
      ex.getMessage should be("parsing default `` for datatype Singleton(Type(primitive,object))")
    }

    it ("should fail for datatype: unit") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", Singleton(Type(Kind.Primitive, Primitives.Unit.toString)))
      }
      ex.getMessage should be("parsing default `` for datatype Singleton(Type(primitive,unit))")
    }

    it ("default: foo, datatype: enum") {
      RubyUtil.rubyDefault("foo", Singleton(Type(Kind.Enum, "test"))) should be("""Test.apply("foo")""")
    }

    it ("should fail for datatype: model") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", Singleton(Type(Kind.Model, "test")))
      }
      ex.getMessage should be("parsing default `` for datatype Singleton(Type(model,test))")
    }

    it ("should fail for datatype: union") {
      val ex = intercept[Exception] {
        RubyUtil.rubyDefault("", Singleton(Type(Kind.Union, "test")))
      }
      ex.getMessage should be("parsing default `` for datatype Singleton(Type(union,test))")
    }

    it ("default: true, datatype: boolean") {
      RubyUtil.rubyDefault("true", Singleton(Type(Kind.Primitive, Primitives.Boolean.toString))) should be("true")
    }

    it ("default: false, datatype: boolean") {
      RubyUtil.rubyDefault("false", Singleton(Type(Kind.Primitive, Primitives.Boolean.toString))) should be("false")
    }

    it ("default: 1234.5, datatype: double") {
      RubyUtil.rubyDefault("1234.5", Singleton(Type(Kind.Primitive, Primitives.Double.toString))) should be("1234.5")
    }

    it ("default: -1234.5, datatype: double") {
      RubyUtil.rubyDefault("-1234.5", Singleton(Type(Kind.Primitive, Primitives.Double.toString))) should be("-1234.5")
    }

    it ("default: 123, datatype: double") {
      RubyUtil.rubyDefault("123", Singleton(Type(Kind.Primitive, Primitives.Double.toString))) should be("123.0")
    }

    it ("default: 124, datatype: integer") {
      RubyUtil.rubyDefault("124", Singleton(Type(Kind.Primitive, Primitives.Integer.toString))) should be("124")
    }

    it ("default: -124, datatype: integer") {
      RubyUtil.rubyDefault("-124", Singleton(Type(Kind.Primitive, Primitives.Integer.toString))) should be("-124")
    }

    it ("default: 124, datatype: long") {
      RubyUtil.rubyDefault("124", Singleton(Type(Kind.Primitive, Primitives.Long.toString))) should be("124")
    }

    it ("default: -124, datatype: long") {
      RubyUtil.rubyDefault("-124", Singleton(Type(Kind.Primitive, Primitives.Long.toString))) should be("-124")
    }

    it ("default: 2014-03-14, datatype: date-iso8601") {
      RubyUtil.rubyDefault("2014-03-14", Singleton(Type(Kind.Primitive, Primitives.DateIso8601.toString))) should be("""Date.parse("2014-03-14")""")
    }

    it ("default: 2014-03-14T12:13:15Z, datatype: date-time-iso8601") {
      RubyUtil.rubyDefault("2014-03-14T12:13:15Z", Singleton(Type(Kind.Primitive, Primitives.DateTimeIso8601.toString))) should be {
        """DateTime.parse("2014-03-14T12:13:15Z")"""
      }
    }

    it ("default: 123453.8, datatype: decimal") {
      RubyUtil.rubyDefault("123453.8", Singleton(Type(Kind.Primitive, Primitives.Decimal.toString))) should be("123453.8")
    }

    it ("default: foo, datatype: string") {
      RubyUtil.rubyDefault("foo", Singleton(Type(Kind.Primitive, Primitives.String.toString))) should be(""""foo"""")
    }

    it ("default: 956169ba-d25e-11e4-8cf3-2bf83a2580ee, datatype: uuid") {
      RubyUtil.rubyDefault("956169ba-d25e-11e4-8cf3-2bf83a2580ee", Singleton(Type(Kind.Primitive, Primitives.Uuid.toString))) should be("""UUID.new("956169ba-d25e-11e4-8cf3-2bf83a2580ee")""")
    }

    it ("""default: ["foo"], datatype: [string]""") {
      RubyUtil.rubyDefault("""["foo"]""", Datatype.List(Type(Kind.Primitive, Primitives.String.toString))) should be("""["foo"]""")
    }

    it ("default: [], datatype: [string]") {
      RubyUtil.rubyDefault("""[]""", Datatype.List(Type(Kind.Primitive, Primitives.String.toString))) should be("""[]""")
    }

    it ("""default: {"name": "foo"}, datatype: map[integer]""") {
      RubyUtil.rubyDefault("""{"name": "foo"}""", Datatype.Map(Type(Kind.Primitive, Primitives.String.toString))) should be("""{"name" => "foo"}""")
    }

    it ("empty map, datatype: map[integer]") {
      RubyUtil.rubyDefault("""{}""", Datatype.Map(Type(Kind.Primitive, Primitives.String.toString))) should be("""{}""")
    }
  }
}
