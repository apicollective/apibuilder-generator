package generator

import lib.{Datatype, Primitives, Type, TypeKind}

sealed trait ScalaPrimitive {
  def name: String
  def asString(originalVarName: String): String
}

object ScalaPrimitive {

  case object Boolean extends ScalaPrimitive {
    def name = "Boolean"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Double extends ScalaPrimitive {
    def name = "Double"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Integer extends ScalaPrimitive {
    def name = "Int"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Long extends ScalaPrimitive {
    def name = "Long"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object DateIso8601 extends ScalaPrimitive {
    def name = "_root_.org.joda.time.LocalDate"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object DateTimeIso8601 extends ScalaPrimitive {
    def name = "_root_.org.joda.time.DateTime"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($varName)"
    }
  }

  case object Decimal extends ScalaPrimitive {
    def name = "Decimal"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case object Object extends ScalaPrimitive {
    def name = "_root_.play.api.libs.json.JsObject"
    def asString(originalVarName: String): String = {
      throw new UnsupportedOperationException(s"unsupported conversion of type object for $originalVarName")
    }
  }

  case object String extends ScalaPrimitive {
    def name = "String"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName"
    }
  }

  case object Unit extends ScalaPrimitive {
    def name = "Unit"
    def asString(originalVarName: String): String = {
      throw new UnsupportedOperationException(s"unsupported conversion of type object for $originalVarName")
    }
  }

  case object Uuid extends ScalaPrimitive {
    def name = "_root_.java.util.UUID"
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case class Model(name: String) extends ScalaPrimitive {
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

  case class Enum(name: String) extends ScalaPrimitive {
    def asString(originalVarName: String): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      s"$varName.toString"
    }
  }

}

sealed trait ScalaDatatype {
  def nilValue: String

  def types: Seq[ScalaPrimitive]

  // TODO: UNION TYPES - change to names: Seq[String] or similar
  def name: String

  def definition(
    originalVarName: String,
    optional: Boolean
  ): String

}

object ScalaDatatype {
  case class List(types: Seq[ScalaPrimitive]) extends ScalaDatatype {
    override def nilValue = "Nil"
    override def name = types.toList match {
      case single :: Nil => s"Seq[${single.name}]"
      case multiple => sys.error("TODO: UNION TYPES")
    }
    override def definition(
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

  case class Map(types: Seq[ScalaPrimitive]) extends ScalaDatatype {
    override def nilValue = "Map.empty"
    override def name = types.toList match {
      case single :: Nil => s"Map[String, ${single.name}]"
      case multiple => sys.error("TODO: UNION TYPES")
    }
    override def definition(
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

  case class Option(types: Seq[ScalaPrimitive]) extends ScalaDatatype {
    override def nilValue = "None"
    override def name = types.toList match {
      case single :: Nil => s"_root_.scala.Option[${single.name}]"
      case multiple => sys.error("TODO: UNION TYPES")
    }
    override def definition(
      originalVarName: String,
      optional: Boolean
    ): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      if (optional) {
        s"$varName: $name" + " = " + nilValue
      } else {
        s"$varName: $name"
      }
    }
  }

  case class Singleton(types: Seq[ScalaPrimitive]) extends ScalaDatatype {
    override def nilValue = "None"
    override def name = {
      types.toList match {
        case single :: Nil => single.name
        case multiple => sys.error("TODO: UNION TYPES")
      }
    }
    override def definition(
      originalVarName: String,
      optional: Boolean
    ): String = {
      val varName = ScalaUtil.quoteNameIfKeyword(originalVarName)
      if (optional) {
        s"$varName: _root_.scala.Option[$name]" + " = " + nilValue
      } else {
        s"$varName: $name"
      }
    }
  }

}

case class ScalaTypeResolver(
  modelPackageName: String,
  enumPackageName: String
) {

  def scalaPrimitive(t: Type): ScalaPrimitive = {
    t match {
      case Type(TypeKind.Primitive, name) => {
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
      case Type(TypeKind.Model, name) => {
        ScalaPrimitive.Model(s"${modelPackageName}.${ScalaUtil.toClassName(name)}")
      }
      case Type(TypeKind.Enum, name) => {
        ScalaPrimitive.Enum(s"${enumPackageName}.${ScalaUtil.toClassName(name)}")
      }
    }
  }

  def scalaDatatype(datatype: Datatype): ScalaDatatype = {
    datatype match {
      case Datatype.List(types) => ScalaDatatype.List(types.map(scalaPrimitive(_)))

      case Datatype.Map(types) => ScalaDatatype.Map(types.map(scalaPrimitive(_)))

      case Datatype.Option(types) => ScalaDatatype.Option(types.map(scalaPrimitive(_)))

      case Datatype.Singleton(types) => ScalaDatatype.Singleton(types.map(scalaPrimitive(_)))
    }
  }

}
