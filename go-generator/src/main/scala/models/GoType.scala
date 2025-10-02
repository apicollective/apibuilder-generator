package go.models

import lib.{Datatype, Text}
import Formatter.*
import play.api.libs.json.*

import scala.annotation.tailrec

case class Klass(name: String) {
  def localName: String = name
  def namespace: Option[String] = None
}

case class GoType(
  importBuilder:ImportBuilder,
  klass: Klass,
  datatype: Datatype
) {

  /**
    * Generates a declaration of the specified value for this datatype
    */
  def declaration(value: String): String = {
    declaration(value, datatype)
  }

  private def declaration(value: String, datatype: Datatype): String = {
    datatype match {
      case Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long => {
        value
      }
      case Datatype.Primitive.Boolean | Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid => {
        // Note we treat booleans as strings so we can differentiate
        // between true, false, and not set
        GoUtil.wrapInQuotes(value)
      }
      case Datatype.Primitive.Object => {
        "nil"
      }
      case Datatype.Primitive.JsonValue => {
        "nil"
      }
      case Datatype.Primitive.Unit => {
        "nil"
      }
      case Datatype.Generated.Model(_) | Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) => {
        throw new UnsupportedOperationException(s"default for type $datatype")
      }
      case Datatype.UserDefined.Enum(name) => {
        val method = importBuilder.publicName(name) + "FromString"
        s"$method(%s)".format(GoUtil.wrapInQuotes(value))
      }
      case Datatype.Container.Option(inner) => {
        declaration(value, inner)
      }
      case Datatype.Container.Map(inner) => {
        val data = Json.parse(value).as[scala.collection.immutable.Map[String, JsValue]].map {
          case (key, v) => GoUtil.wrapInQuotes(key) + ": " + declaration(v.toString, inner)
        }
        data.mkString(s"map[string]{\n", ",\n", ",\n}").table()
      }
      case Datatype.Container.List(inner) => {
        val arr = Json.parse(value).as[JsArray]
        val elements = arr.value.map { v => declaration(v.toString, inner) }
        elements.mkString(s"${klass.localName}{", ",", "}")
      }
    }
  }

  /**
    * Returns true if this type represents the unit type
    */
  def isUnit(): Boolean = {
    isUnit(datatype)
  }

  private def isUnit(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Primitive.Unit => true
      case Datatype.Container.Option(inner) => isUnit(inner)
      case _ => false
    }
  }

  /**
    * Based on the datatype, generates a variable name - e.g. if the
    * type is 'tag', returns tag. If the type is []tag, returns tags
    */
  def classVariableName(): String = {
    classVariableName(datatype)
  }

  private def classVariableName(datatype: Datatype): String = {
    datatype match {
      case _: Datatype.Primitive => "value"
      case Datatype.Generated.Model(name) => importBuilder.privateName(name)
      case Datatype.UserDefined.Model(name) => importBuilder.privateName(name)
      case Datatype.UserDefined.Union(name) => importBuilder.privateName(name)
      case Datatype.UserDefined.Enum(name) => importBuilder.privateName(name)
      case Datatype.Container.Option(inner) => classVariableName(inner)
      case Datatype.Container.Map(inner) => Text.pluralize(classVariableName(inner))
      case Datatype.Container.List(inner) => Text.pluralize(classVariableName(inner))
    }
  }

  def toEscapedString(varName: String): String = {
    val expr = toString(varName)
    expr == varName match {
      case true => {
        val html = importBuilder.ensureImport("html")
        s"${html}.EscapeString($varName)"
      }
      case false => expr
    }
  }

  def toString(varName: String): String = {
    toString(varName, datatype)
  }

  private def toString(varName: String, dt: Datatype): String = {
    dt match {
      case Datatype.Primitive.Boolean => {
        varName
      }
      case Datatype.Primitive.Double => {
        s"${importBuilder.ensureImport("strconv")}.FormatFloat($varName, 'E', -1, 64)"
      }
      case Datatype.Primitive.Integer => {
        s"${importBuilder.ensureImport("strconv")}.FormatInt($varName, 10)"
      }
      case Datatype.Primitive.Long => {
        s"${importBuilder.ensureImport("strconv")}.FormatInt($varName, 10)"
      }
      case Datatype.Primitive.DateIso8601 => varName     // TODO
      case Datatype.Primitive.DateTimeIso8601 => varName // TODO
      case Datatype.Primitive.Decimal => varName
      case Datatype.Primitive.Object => sys.error("Object cannot be converted to escaped string")
      case Datatype.Primitive.JsonValue => sys.error("Json cannot be converted to escaped string")
      case Datatype.Primitive.String => varName
      case Datatype.Primitive.Unit => "nil"
      case Datatype.Primitive.Uuid => varName
      case Datatype.Generated.Model(_) | Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) => {
        sys.error("User defined type cannot be converted to escaped string")
      }
      case Datatype.UserDefined.Enum(_) => {
        s"string($varName)"
      }
      case Datatype.Container.Option(inner) => toString(varName, inner)
      case Datatype.Container.Map(_) | Datatype.Container.List(_) => {
        sys.error("Collections cannot be converted to escaped string")
      }
    }
  }

  def nil(varName: String): String = {
    compareToImplicitValue(varName, datatype, "==")
  }

  def notNil(varName: String): String = {
    compareToImplicitValue(varName, datatype, "!=")
  }

  @tailrec
  private def compareToImplicitValue(varName: String, dt: Datatype, operator: String): String = {
    dt match {
      case Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long => {
        s"0 $operator $varName"
      }
      case Datatype.Primitive.Boolean | Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid | Datatype.UserDefined.Enum(_) => {
        s""""" $operator $varName"""
      }
      case Datatype.Primitive.Object | Datatype.Primitive.JsonValue | Datatype.Primitive.Unit | Datatype.Generated.Model(_) | Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) | Datatype.Container.List(_) => {
        s"""nil $operator $varName"""
      }
      case Datatype.Container.Option(inner) => {
        compareToImplicitValue(varName, inner, operator)
      }
    }
  }
  
}

object GoType {

  def apply(
    importBuilder: ImportBuilder,
    dt: Datatype
  ): GoType = {
    GoType(
      importBuilder = importBuilder,
      klass = klass(importBuilder, dt),
      datatype = dt
    )
  }

  private def klass(importBuilder: ImportBuilder, dt: Datatype): Klass = {
    dt match {
      case Datatype.Primitive.Boolean => Klass("string")
      case Datatype.Primitive.Double => Klass("float64")
      case Datatype.Primitive.Integer => Klass("int32")
      case Datatype.Primitive.Long => Klass("int64")
      case Datatype.Primitive.DateIso8601 => Klass("string")
      case Datatype.Primitive.DateTimeIso8601 => Klass("string")
      case Datatype.Primitive.Decimal => Klass("float64") // TODO. Should we use big/math/Float
      case Datatype.Primitive.Object => Klass("map[string]interface{}")
      case Datatype.Primitive.JsonValue => Klass("interface{}")
      case Datatype.Primitive.String => Klass("string")
      case Datatype.Primitive.Unit => Klass("nil")
      case Datatype.Primitive.Uuid => Klass("string")
      case u: Datatype.UserDefined => Klass(importBuilder.publicName(u.name))
      case u: Datatype.Generated => Klass(importBuilder.publicName(u.name))
      case Datatype.Container.Option(inner) => klass(importBuilder, inner)
      case Datatype.Container.Map(inner) => Klass("map[string]" + klass(importBuilder, inner).localName)
      case Datatype.Container.List(inner) => Klass("[]" + klass(importBuilder, inner).localName)
    }
  }

  @tailrec
  def isNumeric(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Primitive.Double => true
      case Datatype.Primitive.Integer => true
      case Datatype.Primitive.Long => true
      case Datatype.Container.Option(inner) => isNumeric(inner)
      case _ => false
    }
  }
 
}
