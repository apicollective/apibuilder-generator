package scala.models

import lib.Text._
import scala.generator.{Namespaces, PrimitiveWrapper, ScalaDatatype, ScalaModel, ScalaPrimitive, ScalaService, ScalaUnion, ScalaUnionType}

case class Play2JsonCommon(ssd: ScalaService) {

  /**
   * Name of method that converts this datatype to a JsValue
   */
  def getJsonValueMethodName(datatype: ScalaDatatype, varName: String): String = {
    // Boolean, DateIso8601, DateTimeIso8601, Decimal, Double, Integer, List(_), Long, Map(_), Model(_, _), Object, Option(_), String, Union(_, _), Unit, Uuid
    datatype match {
      case ScalaPrimitive.Enum(ns, name) => {
        s"play.api.libs.json.JsString(${varName}.toString)"
      }
      case ScalaPrimitive.Model(ns, name) => {
        toJsonObjectMethodName(ns, name) + s"($varName)"
      }
      case ScalaPrimitive.Union(ns, name) => {
        toJsonObjectMethodName(ns, name) + s"($varName)"
      }
      case ScalaPrimitive.Boolean => {
        s"play.api.libs.json.JsBoolean(${varName})"
      }
      case ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long => {
        s"play.api.libs.json.JsNumber($varName)"
      }
      case ScalaPrimitive.DateIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => {
        s"play.api.libs.json.JsString(${varName}.toString)"
      }
      case ScalaPrimitive.String => {
        s"play.api.libs.json.JsString($varName)"
      }
      case ScalaPrimitive.DateTimeIso8601 => {
        s"play.api.libs.json.JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($varName))"
      }
      case ScalaPrimitive.Object => {
        s"play.api.libs.json.Json.obj($varName)"
      }
      case ScalaDatatype.List(_) | ScalaDatatype.Map(_) | ScalaDatatype.Option(_) | ScalaPrimitive.Unit => {
        s"play.api.libs.json.Json.toJson($varName)"
      }
    }
  }

  /**
   * Name of method that converts this datatype to a JsObject, if
   * possible.
   */
  def getJsonObjectMethodName(datatype: ScalaDatatype, varName: String): Option[String] = {
    datatype match {
      case ScalaPrimitive.Model(ns, name) => {
        Some(toJsonObjectMethodName(ns, name) + s"($varName)")
      }
      case ScalaPrimitive.Union(ns, name) => {
        Some(toJsonObjectMethodName(ns, name) + s"($varName)")
      }
      case ScalaPrimitive.Enum(ns, name) => {
        None
      }
      case x: ScalaDatatype.Container => {
        None
      }
      case x: ScalaPrimitive => {
        None
      }
    }
  }

  def toJsonObjectMethodName(ns: Namespaces, name: String): String = {
    val method = s"json${ssd.name}${name}ToJsonObject"
    ns.base == ssd.namespaces.base match {
      case true => method
      case false => Seq(ns.json, method).mkString(".")
    }
  }

  def implicitWriter(name: String, qualifiedName: String, methodName: String) = {
    Seq(
      s"${implicitWriterDef(name)} = {",
      s"  new play.api.libs.json.Writes[$qualifiedName] {",
      s"    def writes(obj: $qualifiedName) = {",
      s"      $methodName(obj)",
      "    }",
      "  }",
      "}"
    ).mkString("\n")
  }

  def implicitReaderDef(name: String): String = {
    s"implicit def ${implicitReaderName(name)}: play.api.libs.json.Reads[$name]"
  }

  def implicitReaderName(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"jsonReads${ssd.name}$name"
  }

  def implicitWriterDef(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"implicit def jsonWrites${ssd.name}$name: play.api.libs.json.Writes[$name]"
  }
  
}

case class Play2Json(
  ssd: ScalaService
) {

  private[this] val play2JsonCommon = Play2JsonCommon(ssd)

  def generate(): String = {
    Seq(
      ssd.models.map(readersAndWriters(_)).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => readers(w.model)).mkString("\n\n"),
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
      s"${play2JsonCommon.implicitReaderDef(union.name)} = {",
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
      s"${play2JsonCommon.implicitReaderDef(union.name)} = new play.api.libs.json.Reads[${union.name}] {",
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
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, union.name)

    Seq(
      union.discriminator match {
        case None => writersWithoutDiscriminator(union)
        case Some(discriminator) => writersWithDiscriminator(union, discriminator)
      },
      play2JsonCommon.implicitWriter(union.name, union.qualifiedName, method)
    ).mkString("\n\n")
  }

  private[models] def writersWithoutDiscriminator(union: ScalaUnion): String = {
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, union.name)

    Seq(
      s"def $method(obj: ${union.qualifiedName}) = {",
      s"  obj match {",
      unionTypesWithNames(union).map { case (t, typeName) =>
        val json = play2JsonCommon.getJsonValueMethodName(t.datatype, "x")
        s"""case x: ${typeName} => play.api.libs.json.Json.obj("${t.originalName}" -> $json)"""
      }.mkString("\n").indent(4),
      s"""    case x: ${union.undefinedType.fullName} => sys.error(s"The type[${union.undefinedType.fullName}] should never be serialized")""",
      "  }",
      "}"
    ).mkString("\n")
  }

  private[models] def writersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, union.name)

    Seq(
      s"def $method(obj: ${union.qualifiedName}) = {",
      Seq(
        "obj match {",
        Seq(
          unionTypesWithNames(union).map { case (t, typeName) =>
            play2JsonCommon.getJsonObjectMethodName(t.datatype, "x") match {
              case Some(json) => {
                s"""case x: ${typeName} => $json ++ play.api.libs.json.Json.obj("$discriminator" -> "${t.originalName}")"""
              }
              case None => {
                val json = t.datatype match {
                  case ScalaPrimitive.Enum(ns, name) => {
                    s"play.api.libs.json.JsString(x.toString)"
                  }
                  case x: ScalaPrimitive => {
                    // support wrapped primitives
                    s"play.api.libs.json.Json.toJson(x.value)"
                  }
                  case _ => {
                    play2JsonCommon.getJsonValueMethodName(t.datatype, "x")
                  }
                }

                Seq(
                  s"case x: ${typeName} => play.api.libs.json.Json.obj(",
                  s"""  "$discriminator" -> "${t.originalName}",""",
                  s"""  "${PrimitiveWrapper.FieldName}" -> $json""",
                  s")"
                ).mkString("\n")
              }
            }
          }.mkString("\n"),
          s"case other => {",
          """  sys.error(s"The type[${other.getClass.getName}] has no JSON writer")""",
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n")
  }

  private def reader(union: ScalaUnion, ut: ScalaUnionType): String = {
    ut.model match {
      case Some(model) => {
        play2JsonCommon.implicitReaderName(ut.name)
      }
      case None => {
        ut.enum match {
          case Some(enum) => {
            play2JsonCommon.implicitReaderName(ut.name)
          }
          case None => {
            ut.datatype match {
              // TODO enum representation should be refactored
              // so that the type can be read directly from
              // the enum (not ut). The enum type is always
              // a primitive, so this match is redundant,
              // but necessary due to the way the data is currently
              // structured
              case p: ScalaPrimitive => {
                play2JsonCommon.implicitReaderName(PrimitiveWrapper.className(union, p))
              }
              case dt => sys.error(s"unsupported datatype[${dt}] in union ${ut}")
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
      s"${play2JsonCommon.implicitReaderDef(model.name)} = {",
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
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, model.name)

    val base = Seq(
      Seq(
        s"def $method(obj: ${model.qualifiedName}) = {",
        Seq(
          "play.api.libs.json.Json.obj(",
          model.fields.map { field =>
            val json = play2JsonCommon.getJsonValueMethodName(field.datatype, s"obj.${field.name}")
            s""""${field.originalName}" -> $json"""
          }.mkString(",\n").indent(2),
          ")"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n")
    ).mkString("\n\n")

    ssd.unionsForModel(model) match {
      case Nil => {
        Seq(
          base,
          play2JsonCommon.implicitWriter(model.name, model.qualifiedName, method)
        ).mkString("\n\n")
      }
      case _ => {
        // Let the implicit for the associated union handle the serialization
        base
      }
    }
  }

  private def unionTypesWithNames(union: ScalaUnion): Seq[(ScalaUnionType, String)] = {
    union.types.map { t =>
      (t,
        t.datatype match {
          case p @ (ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _)) => {
            p.name
          }
          case p: ScalaPrimitive => ssd.modelClassName(PrimitiveWrapper.className(union, p))
          case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encountered in union ${union.name}")
        }
      )
    }
  }
}
