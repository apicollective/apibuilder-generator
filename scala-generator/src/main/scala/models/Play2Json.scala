package scala.models

import lib.Text._
import scala.generator.{PrimitiveWrapper, ScalaDatatype, ScalaModel, ScalaPrimitive, ScalaService, ScalaUnion, ScalaUnionType}

case class Play2Json(
  ssd: ScalaService
) {

  private sealed trait ReadWrite
  private case object Reads extends ReadWrite { override def toString = "Reads" }
  private case object Writes extends ReadWrite { override def toString = "Writes" }

  def generate(): String = {
    Seq(
      ssd.models.map(readersAndWriters(_)).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => readersAndWriters(w.model)).mkString("\n\n"),
      ssd.unions.map(readersAndWriters(_)).mkString("\n\n")
    ).filter(!_.trim.isEmpty).mkString("\n\n")    
  }

  private def readersAndWriters(union: ScalaUnion): String = {
    readers(union) + "\n\n" + writers(union)
  }

  private[models] def readers(union: ScalaUnion): String = {
    union.discriminator match {
      case None => readersWithoutDiscriminator(union)
      case Some(discriminator) => readersWithDiscriminator(union, discriminator)
    }
  }

  private[this] def readersWithoutDiscriminator(union: ScalaUnion): String = {
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

  private[this] def readersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    Seq(
      s"${identifier(union.name, Reads)} = new play.api.libs.json.Reads[${union.name}] {",
      Seq(s"def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[${union.name}] = {",
        Seq(s"""(js \\ "$discriminator").validate[String] match {""",
          Seq(
            """case play.api.libs.json.JsError(msg) => play.api.libs.json.JsError(msg)""",
            """case play.api.libs.json.JsSuccess(discriminator, _) => {""",
            Seq(
              """discriminator match {""",
              unionTypesWithNames(union).map { case (t, typeName) =>
                s"""case "${t.originalName}" => js.validate[$typeName]"""
              }.mkString("\n").indent(2),
              s"""case other => play.api.libs.json.JsSuccess(${union.undefinedType.fullName}(other))""".indent(2),
              "}"
            ).mkString("\n").indent(2),
            "}"
          ).mkString("\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n")
  }

  private[models] def writers(union: ScalaUnion): String = {
    union.discriminator match {
      case None => writersWithoutDiscriminator(union)
      case Some(discriminator) => writersWithDiscriminator(union, discriminator)
    }
  }

  private[models] def writersWithoutDiscriminator(union: ScalaUnion): String = {
    Seq(
      s"${identifier(union.name, Writes)} = new play.api.libs.json.Writes[${union.name}] {",
      s"  def writes(obj: ${union.qualifiedName}) = obj match {",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""case x: ${typeName} => play.api.libs.json.Json.obj("${t.originalName}" -> ${writer("x", union, t)})"""
      }.mkString("\n").indent(4),
      s"""    case x: ${union.undefinedType.fullName} => sys.error(s"The type[${union.undefinedType.fullName}] should never be serialized")""",
      "  }",
      "}"
    ).mkString("\n")
  }

  private[models] def writersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
  //  implicit val expandableUserWrites = new Writes[ExpandableUser] {
  //    def writes(x: ExpandableUser) = {
  //      x match {
  //       case x: UserReference => Json.obj(
  //         "discriminator" -> "user_reference",
  //         "id" -> x.id
  //       )
  //       case x: User => Json.obj(
  //         "discriminator" -> "user",
  //         "id" -> x.id,
  //         "email" -> x.email
  //       )
  //     }
  //   }
  // }

    Seq(
      s"${identifier(union.name, Writes)} = new play.api.libs.json.Writes[${union.name}] {",
      Seq(
        s"def writes(obj: ${union.qualifiedName}) = {",
        Seq(
          s"obj match {",
          Seq(
            unionTypesWithNames(union).map { case (t, typeName) =>
              s"""case x: ${typeName} => """ + toJsonObject(union, t, "x", discriminator)
            }.mkString("\n").indent(1),
            s"""  case x: ${union.undefinedType.fullName} => sys.error(s"The type[${union.undefinedType.fullName}] should never be serialized")"""
          ).mkString("\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n")
  }

  private def toJsonObject(union: ScalaUnion, t: ScalaUnionType, varName: String, discriminator: String): String = {
    Seq(
      "play.api.libs.json.Json.obj(",
      Seq(
        s""""$discriminator" -> "${t.originalName}",""",
        t.datatype match {
          case ScalaPrimitive.Model(ns, name) => {
            val model = ssd.models.find(_.name == name).getOrElse {
              sys.error(s"Could not find model[$name]")
            }
            model.fields.map { field =>
              s""""${field.originalName}" -> play.api.libs.json.Json.toJson($varName.${field.name})"""
            }.mkString(",\n")
          }
          case ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _) => {
            s""""${PrimitiveWrapper.FieldName}" -> play.api.libs.json.Json.toJson($varName)"""
          }
          case p: ScalaPrimitive => {
            s""""${PrimitiveWrapper.FieldName}" -> play.api.libs.json.Json.toJson(${varName}.value)"""
          }
          case c: ScalaDatatype.Container => {
            sys.error(s"unsupported container type ${c} encountered in union ${union.name}")
          }
      }
      ).mkString("\n").indent(2),
      ")"
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

  private def writer(varName: String, union: ScalaUnion, ut: ScalaUnionType): String = {
    ut.model match {
      case Some(model) => methodName(model.name, Writes) + ".writes(x)"
      case None => {
        ut.enum match {
          case Some(enum) => methodName(enum.name, Writes) + ".writes(x)"
          case None => ut.datatype match {
            case p: ScalaPrimitive => methodName(PrimitiveWrapper.className(union, p), Writes) + ".writes(x)"
            case dt => sys.error(s"unsupported datatype[${dt}] in union ${ut}")
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
        Seq(
          s"${identifier(model.name, Writes)} = new play.api.libs.json.Writes[${model.name}] {",
          s"  def writes(x: ${model.name}) = play.api.libs.json.Json.obj(",
          s"""    "${field.originalName}" -> play.api.libs.json.Json.toJson(x.${field.name})""",
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
  ): String = {
    s"json$readWrite${ssd.name}$name"
  }

  private def unionTypesWithNames(union: ScalaUnion): Seq[(ScalaUnionType, String)] = {
    union.types.map { t =>
      (t,
        t.datatype match {
          case p @ (ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _)) => {
            p.name
          }
          case p: ScalaPrimitive => PrimitiveWrapper.className(union, p)
          case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encountered in union ${union.name}")
        }
      )
    }
  }
}
