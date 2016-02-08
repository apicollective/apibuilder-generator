package go.models

import lib.{Datatype, Text}

sealed trait Klass {

  def localName: String

  def namespace: Option[String]

}

object Klass {

  case class Root(name: String) extends Klass {
    override def localName = name
    override def namespace: Option[String] = None
  }

  case class Import(ns: String, alias: String, name: String) extends Klass {
    override def localName = s"${alias}.$name"
    override def namespace: Option[String] = Some(ns)
  }

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

  private[this] def declaration(value: String, datatype: Datatype): String = {
    datatype match {
      case Datatype.Primitive.Boolean | Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long => {
        value
      }
      case Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid => {
        GoUtil.wrapInQuotes(value)
      }
      case Datatype.Primitive.Object => "interface{}"
      case Datatype.Primitive.Unit => "nil"
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) => "nil"
      case Datatype.UserDefined.Enum(_) => value
      case Datatype.Container.Option(inner) => declaration(value, inner)
      case Datatype.Container.Map(inner) => {
        // TODO: Parse value
        s"${klass.localName}{ " + declaration(value, inner) + " }"
      }
      case Datatype.Container.List(inner) => {
        // TODO: Parse value
        s"${klass.localName}{ " + declaration(value, inner) + " }"
      }
    }
  }

  /**
    * Returns true if this type represents the unit type
    */
  def isUnit(): Boolean = {
    isUnit(datatype)
  }

  private[this] def isUnit(datatype: Datatype): Boolean = {
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

  private[this] def classVariableName(datatype: Datatype): String = {
    datatype match {
      case p: Datatype.Primitive => "value"
      case Datatype.UserDefined.Model(name) => GoUtil.privateName(name)
      case Datatype.UserDefined.Union(name) => GoUtil.privateName(name)
      case Datatype.UserDefined.Enum(name) => "value"
      case Datatype.Container.Option(inner) => classVariableName(inner)
      case Datatype.Container.Map(inner) => GoUtil.privateName(Text.pluralize(classVariableName(inner)))
      case Datatype.Container.List(inner) => GoUtil.privateName(Text.pluralize(classVariableName(inner)))
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

  private[this] def toString(varName: String, dt: Datatype): String = {
    dt match {
      case Datatype.Primitive.Boolean => {
        val strconv = importBuilder.ensureImport("strconv")
        s"${strconv}.FormatBool($varName)"
      }
      case Datatype.Primitive.Double => {
        val strconv = importBuilder.ensureImport("strconv")
        s"${strconv}.FormatFloat($varName, 'E', -1, 64)"
      }
      case Datatype.Primitive.Integer => {
        val strconv = importBuilder.ensureImport("strconv")
        s"${strconv}.FormatInt($varName, 10)"
      }
      case Datatype.Primitive.Long => {
        val strconv = importBuilder.ensureImport("strconv")
        s"${strconv}.FormatInt($varName, 10)"
      }
      case Datatype.Primitive.DateIso8601 => varName     // TODO
      case Datatype.Primitive.DateTimeIso8601 => varName // TODO
      case Datatype.Primitive.Decimal => varName
      case Datatype.Primitive.Object => sys.error("Object cannot be converted to escaped string")
      case Datatype.Primitive.String => varName
      case Datatype.Primitive.Unit => "nil"
      case Datatype.Primitive.Uuid => varName
      case u: Datatype.UserDefined => {
        u match {
          case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) => {
            sys.error("User defined type cannot be converted to escaped string")
          }
          case Datatype.UserDefined.Enum(name) => {
            varName
          }
        }
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

  private[this] def compareToImplicitValue(varName: String, dt: Datatype, operator: String): String = {
    dt match {
      case Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long => {
        s"0 $operator $varName"
      }
      case Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid | Datatype.UserDefined.Enum(_) => {
        s""""" $operator $varName"""
      }
      case Datatype.Primitive.Boolean | Datatype.Primitive.Object | Datatype.Primitive.Unit | Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) | Datatype.Container.List(_) => {
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

  private[this] def klass(importBuilder: ImportBuilder, dt: Datatype): Klass = {
    dt match {
      case Datatype.Primitive.Boolean => Klass.Root("bool")
      case Datatype.Primitive.Double => Klass.Root("float64")
      case Datatype.Primitive.Integer => Klass.Root("int32")
      case Datatype.Primitive.Long => Klass.Root("int64")
      case Datatype.Primitive.DateIso8601 => Klass.Root("string")
      case Datatype.Primitive.DateTimeIso8601 => Klass.Root("string")
      case Datatype.Primitive.Decimal => Klass.Root("string")
      case Datatype.Primitive.Object => Klass.Root("interface{}")
      case Datatype.Primitive.String => Klass.Root("string")
      case Datatype.Primitive.Unit => Klass.Root("nil")
      case Datatype.Primitive.Uuid => Klass.Root("string")
      case u: Datatype.UserDefined => {
        val i = u.name.lastIndexOf(".")
        (i > 0) match {
          case true => {
            val ns = u.name.substring(0, i)
            val name = u.name.substring(i+1)

            val alias = importBuilder.ensureImport(ns)
            Klass.Import(ns, alias, GoUtil.publicName(name))
          }
          case false => {
            Klass.Root(GoUtil.publicName(u.name))
          }
        }
      }
      case Datatype.Container.Option(inner) => klass(importBuilder, inner)
      case Datatype.Container.Map(inner) => Klass.Root("map[string]" + klass(importBuilder, inner).localName)
      case Datatype.Container.List(inner) => Klass.Root("[]" + klass(importBuilder, inner).localName)
    }
  }

  def isNumeric(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Primitive.Double => true
      case Datatype.Primitive.Integer => true
      case Datatype.Primitive.Long => true
      case Datatype.Container.Option(inner) => isNumeric(inner)
      case _ => false
    }
  }
  
  def isBoolean(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Primitive.Boolean => true
      case Datatype.Container.Option(inner) => isBoolean(inner)
      case _ => false
    }
  }
  
 
}
