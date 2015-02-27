package generator

import lib.{Datatype, Primitives, Type, Kind}

sealed trait ScalaPrimitive {
  def apidocType: String
  def shortName: String
  def asString(originalVarName: String): String
  def namespace: Option[String] = None
  def fullName: String = namespace match {
    case None => shortName
    case Some(ns) => s"$ns.$shortName"
  }
}

object ScalaPrimitive {

  case object Boolean extends ScalaPrimitive {
    def apidocType = "boolean"
    def shortName = "Boolean"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Double extends ScalaPrimitive {
    def apidocType = "double"
    def shortName = "Double"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Integer extends ScalaPrimitive {
    def apidocType = "integer"
    def shortName = "Int"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Long extends ScalaPrimitive {
    def apidocType = "long"
    def shortName = "Long"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object DateIso8601 extends ScalaPrimitive {
    override def namespace = Some("_root_.org.joda.time")
    def apidocType = "date-iso8601"
    def shortName = "LocalDate"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object DateTimeIso8601 extends ScalaPrimitive {
    override def namespace = Some("_root_.org.joda.time")
    def apidocType = "date-time-iso8601"
    def shortName = "DateTime"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($varName)"
    }
  }

  case object Decimal extends ScalaPrimitive {
    def apidocType = "decimal"
    def shortName = "BigDecimal"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Object extends ScalaPrimitive {
    override def namespace = Some("_root_.play.api.libs.json")
    def apidocType = "object"
    def shortName = "JsObject"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object String extends ScalaPrimitive {
    def apidocType = "string"
    def shortName = "String"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName"
    }
  }

  case object Unit extends ScalaPrimitive {
    def apidocType = "unit"
    def shortName = "Unit"
    def asString(originalVarName: String): String = {
      throw new UnsupportedOperationException(s"unsupported conversion of type object for $originalVarName")
    }
  }

  case object Uuid extends ScalaPrimitive {
    override def namespace = Some("_root_.java.util")
    def apidocType = "uuid"
    def shortName = "UUID"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case class Model(ns: String, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(ns)
    def apidocType = shortName
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case class Enum(ns: String, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(ns)
    def apidocType = shortName
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case class Union(ns: String, shortName: String) extends ScalaPrimitive {
    override def namespace = Some(ns)
    def apidocType = shortName
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

}

sealed trait ScalaDatatype {
  def nilValue: String

  def primitive: ScalaPrimitive

  def name: String

  def clientDefinition(
    originalVarName: String,
    optional: Boolean
  ): String = {
    val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
    if (optional) {
      s"$varName: _root_.scala.Option[$name] = None"
    } else {
      s"$varName: $name"
    }
  }

  /**
    * For collections, the server side optional definition uses the
    * nil value - e.g. a List will be Nil (instead of None).
    */
  def serverDefinition(
    originalVarName: String,
    optional: Boolean
  ): String = {
    val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
    if (optional) {
      s"$varName: $name = " + nilValue
    } else {
      s"$varName: $name"
    }
  }

}

object ScalaDatatype {

  case class List(primitive: ScalaPrimitive) extends ScalaDatatype {
    override def nilValue = "Nil"
    override def name = s"Seq[${primitive.fullName}]"
  }

  case class Map(primitive: ScalaPrimitive) extends ScalaDatatype {
    override def nilValue = "Map.empty"
    override def name = s"Map[String, ${primitive.fullName}]"
  }

  case class Singleton(primitive: ScalaPrimitive) extends ScalaDatatype {
    override def nilValue = "None"
    override def name = primitive.fullName

    /**
      * For singleton, server definition matches client definition
      * wrapping the type in an Option always.
      */
    override def serverDefinition(
      originalVarName: String,
      optional: Boolean
    ): String = clientDefinition(originalVarName, optional)

  }

}

case class ScalaTypeResolver(
  namespaces: Namespaces
) {

  def scalaPrimitive(t: Type): ScalaPrimitive = {
    t match {
      case Type(Kind.Primitive, name) => {
        Primitives(name).getOrElse {
          sys.error(s"Invalid primitive[$name]")
        } match {
          case Primitives.Boolean => ScalaPrimitive.Boolean
          case Primitives.Decimal => ScalaPrimitive.Decimal
          case Primitives.Integer => ScalaPrimitive.Integer
          case Primitives.Double => ScalaPrimitive.Double
          case Primitives.Long => ScalaPrimitive.Long
          case Primitives.Object => ScalaPrimitive.Object
          case Primitives.String => ScalaPrimitive.String
          case Primitives.DateIso8601 => ScalaPrimitive.DateIso8601
          case Primitives.DateTimeIso8601 => ScalaPrimitive.DateTimeIso8601
          case Primitives.Uuid => ScalaPrimitive.Uuid
          case Primitives.Unit => ScalaPrimitive.Unit
        }
      }
      case Type(Kind.Model, name) => {
        name.split("\\.").toList match {
          case n :: Nil => ScalaPrimitive.Model(namespaces.models, ScalaUtil.toClassName(n))
          case multiple => {
            val (ns, n) = parseQualifiedName(namespaces.models, name)
            ScalaPrimitive.Model(ns, n)
          }
        }
      }
      case Type(Kind.Enum, name) => {
        val (ns, n) = parseQualifiedName(namespaces.enums, name)
        ScalaPrimitive.Enum(ns, n)
      }
      case Type(Kind.Union, name) => {
        val (ns, n) = parseQualifiedName(namespaces.unions, name)
        ScalaPrimitive.Union(ns, n)
      }
    }
  }

  /**
   * If name is a qualified name (as identified by having a dot),
   * parses the name and returns a tuple of (namespace,
   * name). Otherwise, returns (defaultNamespace, name)
   */
  private def parseQualifiedName(defaultNamespace: String, name: String): (String, String) = {
    name.split("\\.").toList match {
      case n :: Nil => (defaultNamespace, ScalaUtil.toClassName(name))
      case multiple => 
        val n = multiple.last
        val ns = multiple.reverse.drop(1).reverse.mkString(".")
        (Namespaces.quote(ns), ScalaUtil.toClassName(n))
    }
  }

  def scalaDatatype(datatype: Datatype): ScalaDatatype = {
    datatype match {
      case Datatype.List(t) => ScalaDatatype.List(scalaPrimitive(t))
      case Datatype.Map(t) => ScalaDatatype.Map(scalaPrimitive(t))
      case Datatype.Singleton(t) => ScalaDatatype.Singleton(scalaPrimitive(t))
    }
  }

}
