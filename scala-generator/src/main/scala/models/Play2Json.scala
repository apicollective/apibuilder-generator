package scala.models

import lib.Text._
import scala.generator.{PrimitiveWrapper, ScalaDatatype, ScalaEnum, ScalaModel, ScalaPrimitive, ScalaService, ScalaUnion, ScalaUnionType}

case class Play2Json(
  ssd: ScalaService
) {

  private sealed trait ReadWrite
  private case object Reads extends ReadWrite { override def toString = "Reads" }
  private case object Writes extends ReadWrite { override def toString = "Writes" }

  def generate(): String = {
    Seq(
      ssd.enums.map(conversions(_)).mkString("\n\n"),
      ssd.models.map(readersAndWriters(_)).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => readersAndWriters(w.model)).mkString("\n\n"),
      ssd.unions.map(readersAndWriters(_)).mkString("\n\n")
    ).filter(!_.trim.isEmpty).mkString("\n\n")    
  }

  private def conversions(enum: ScalaEnum): String = {
    s"implicit def convert${enum.name}ToString(value: ${enum.qualifiedName}) = value.toString"
  }

  private def readersAndWriters(union: ScalaUnion): String = {
    readers(union) + "\n\n" + writers(union)
  }

  private[models] def readers(union: ScalaUnion): String = {
    Seq(
      s"${identifier(union.name, Reads)} = {",
      s"  (",
      union.types.map { scalaUnionType =>
        s"""(__ \\ "${scalaUnionType.originalName}").read(${reader(union, scalaUnionType)}).asInstanceOf[play.api.libs.json.Reads[${union.name}]]"""
      }.mkString("\norElse\n").indent(4),
      s"    orElse",
      s"    play.api.libs.json.Reads(jsValue => play.api.libs.json.JsSuccess(${union.undefinedType.name}(jsValue.toString))).asInstanceOf[play.api.libs.json.Reads[${union.name}]]",
      s"  )",
      s"}"
    ).mkString("\n")
  }

  private[models] def writers(union: ScalaUnion): String = {
    Seq(
      s"${identifier(union.name, Writes)} = new play.api.libs.json.Writes[${union.name}] {",
      s"  def writes(obj: ${union.name}) = obj match {",
      union.types.flatMap { t =>
        val typeName = t.datatype match {
          case p @ (ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _)) => {
            p.name
          }
          case p: ScalaPrimitive => PrimitiveWrapper.className(union, p)
          case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encountered in union ${union.name}")
        }
        writer("x", union, t).map { writerMethod =>
          s"""case x: ${typeName} => play.api.libs.json.Json.obj("${t.originalName}" -> $writerMethod)"""
        }
      }.mkString("\n").indent(4),
      s"""    case x: ${union.undefinedType.fullName} => sys.error(s"The type[${union.undefinedType.fullName}] should never be serialized")""",
      "  }",
      "}"
    ).mkString("\n")
  }

  private def reader(union: ScalaUnion, ut: ScalaUnionType): String = {
    ut.model match {
      case Some(model) => methodName(model.name, Reads)
      case None => {
        ut.enum match {
          case Some(enum) => methodName(enum.name, Reads)
          case None => ut.datatype match {
            // TODO enum representation should be refactored
            // so that the type can be read directly from
            // the enum (not ut). The enum type is always
            // a primitive, so this match is redundant,
            // but necessary due to the way the data is currently
            // structured
            case p: ScalaPrimitive => methodName(PrimitiveWrapper.className(union, p), Reads)
            case dt => sys.error(s"unsupported datatype[${dt}] in union ${ut}")
          }
        }
      }
    }
  }

  private def writer(varName: String, union: ScalaUnion, ut: ScalaUnionType): Option[String] = {
    ut.model match {
      case Some(model) => {
        Some(methodName(model.name, Writes) + ".writes(x)")
      }
      case None => {
        ut.enum match {
          case Some(enum) => {
            // No writer - we have an implicit enum => string conversion and
            // then just use the native string json writer
            None
          }
          case None => ut.datatype match {
            case p: ScalaPrimitive => Some(
              methodName(PrimitiveWrapper.className(union, p), Writes) + ".writes(x)"
            )
            case dt => {
              None
            }
          }
        }
      }
    }
  }

  private[models] def readersAndWriters(model: ScalaModel): String = {
    readers(model) ++ "\n\n" ++ writers(model)
  }

  private[models] def readers(model: ScalaModel): String = {
    Seq(
      s"${identifier(model.name, Reads)} = {",
      fieldReaders(model).indent(2),
      s"}"
    ).mkString("\n")
  }

  private[models] def fieldReaders(model: ScalaModel): String = {
    val serializations = model.fields.map { field =>
      field.datatype match {
        case ScalaDatatype.Option(inner) => {
          s"""(__ \\ "${field.originalName}").readNullable[${inner.name}]"""
        }
        case datatype => {
          s"""(__ \\ "${field.originalName}").read[${datatype.name}]"""
        }
      }
    }

    model.fields match {
      case field :: Nil => {
        serializations.head + s""".map { x => new ${model.name}(${field.name} = x) }"""
      }
      case fields => {
        Seq(
          "(",
          serializations.mkString(" and\n").indent(2),
          s")(${model.name}.apply _)"
        ).mkString("\n")
      }
    }
  }

  private[models] def writers(model: ScalaModel): String = {
    model.fields match {
      case field :: Nil => {
        val writer = field.datatype match {
          case ScalaPrimitive.Enum(_, _) => s"play.api.libs.json.JsString(x.${field.name}.toString)"
          case _ => s"play.api.libs.json.Json.toJson(x.${field.name})"
        }

        Seq(
          s"${identifier(model.name, Writes)} = new play.api.libs.json.Writes[${model.name}] {",
          s"  def writes(x: ${model.name}) = play.api.libs.json.Json.obj(",
          s"""    "${field.originalName}" -> $writer""",
          "  )",
          "}"
        ).mkString("\n")
      }

      case fields => {
        Seq(
          s"${identifier(model.name, Writes)} = {",
          s"  (",
          model.fields.map { field =>
            field.datatype match {
              case ScalaDatatype.Option(inner) =>
                s"""(__ \\ "${field.originalName}").writeNullable[${inner.name}]"""
              case datatype =>
                s"""(__ \\ "${field.originalName}").write[${datatype.name}]"""
            }
          }.mkString(" and\n").indent(4),
          s"  )(unlift(${model.name}.unapply _))",
          s"}"
        ).mkString("\n")
      }
    }
  }

  private[models] def identifier(
    name: String,
    readWrite: ReadWrite
  ): String = {
    val method = methodName(name, readWrite)
    s"implicit def $method: play.api.libs.json.$readWrite[$name]"
  }

  private def methodName(
    name: String,
    readWrite: ReadWrite
  ): String = s"json$readWrite${ssd.name}$name"

}
