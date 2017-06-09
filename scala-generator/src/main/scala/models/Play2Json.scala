package scala.models

import lib.Text._

import scala.generator.ScalaPrimitive.{Model, Union}
import scala.generator._

case class Play2JsonCommon(ssd: ScalaService) {

  /**
    * Never use the service name in the method name. We never import
    * this method implicitly and we need to be able to reference
    * method names defined for other services.
    */
  def toJsonObjectMethodName(ns: Namespaces, name: String): String = {
    val method = s"jsObject${name}"
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

  def implicitWriterName(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"jsonWrites${ssd.name}$name"
  }

  def implicitWriterDef(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    val methodName = implicitWriterName(name)
    s"implicit def $methodName: play.api.libs.json.Writes[$name]"
  }
  
}

case class Play2Json(
  ssd: ScalaService
) {

  private[this] val play2JsonCommon = Play2JsonCommon(ssd)

  def generateModelsAndUnions(): String = {
    Seq(
      ssd.models.map(readersAndWriters(_)).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => readers(w.model)).mkString("\n\n"),
      ssd.unions.map(readersAndWriters(_)).mkString("\n\n")
    ).filter(!_.trim.isEmpty).mkString("\n\n")    
  }

  /**
    * Returns the implicits for enum json serialization, handling
    * conversion both from the string and object representations.
    */
  def generateEnums(): String = {
    ssd.enums.map(enumReadersAndWriters(_)).mkString("\n\n")
  }

  private[models] def enumReadersAndWriters(enum: ScalaEnum): String = {
    val jsObjectWriterMethod = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, enum.name)
    val jsValueWriterMethod = play2JsonCommon.implicitWriterName(enum.name)
    val implicitWriter = play2JsonCommon.implicitWriter(enum.name, enum.qualifiedName, jsValueWriterMethod)

    Seq(
      s"implicit val jsonReads${ssd.name}${enum.name} = new play.api.libs.json.Reads[${enum.qualifiedName}] {",
      Seq(
        s"def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[${enum.qualifiedName}] = {",
        Seq(
          "js match {",
          Seq(
            s"case v: play.api.libs.json.JsString => play.api.libs.json.JsSuccess(${enum.qualifiedName}(v.value))",
            "case _ => {",
            Seq(
              """(js \ "value").validate[String] match {""",
              Seq(
                s"case play.api.libs.json.JsSuccess(v, _) => play.api.libs.json.JsSuccess(${enum.qualifiedName}(v))",
                "case err: play.api.libs.json.JsError => err"
              ).mkString("\n").indent(2),
              "}"
            ).mkString("\n").indent(2),
            "}"
          ).mkString("\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}",
      "",
      s"def $jsValueWriterMethod(obj: ${enum.qualifiedName}) = {",
      s"""  play.api.libs.json.JsString(obj.toString)""",
      s"}",
      "",
      s"def $jsObjectWriterMethod(obj: ${enum.qualifiedName}) = {",
      s"""  play.api.libs.json.Json.obj("${PrimitiveWrapper.FieldName}" -> play.api.libs.json.JsString(obj.toString))""",
      s"}",
      "",
      implicitWriter
    ).mkString("\n")
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
      s"def $method(obj: ${union.qualifiedName}): play.api.libs.json.JsObject = {",
      s"  obj match {",
      unionTypesWithNames(union).map { case (t, typeName) =>
        val json = getJsonValueForUnion(t.datatype, "x")
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
      s"def $method(obj: ${union.qualifiedName}): play.api.libs.json.JsObject = {",
      Seq(
        "obj match {",
        Seq(
          unionTypesWithNames(union).map { case (t, typeName) =>
            val json = getJsonValueForUnion(t.datatype, "x", Some(Discriminator(discriminator, t.originalName)))
            s"case x: ${typeName} => $json"
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
    def findModelByName(name: String): Option[ScalaModel] =
      ssd.models.find(_.qualifiedName == name)

    def findUnionByName(name: String): Option[ScalaUnion] =
      ssd.unions.find(_.qualifiedName == name)

    /** Does field have reference somewhere down the tree to model? */
    def hasReferenceToModel(datatype: ScalaDatatype, visited: List[ScalaDatatype] = Nil): Boolean = {
      if (visited contains datatype) false
      else {
        datatype match {
          case ScalaDatatype.Option(inner) => hasReferenceToModel(inner, datatype :: visited)
          case ScalaDatatype.List(inner) => hasReferenceToModel(inner, datatype :: visited)
          case ScalaDatatype.Map(inner) => hasReferenceToModel(inner, datatype :: visited)
          case mtype: Model =>
            findModelByName(mtype.fullName).fold(false) { m =>
              if (m == model) // TODO do the classes share a single instance of ScalaModel?
                true
              else
                m.fields.map(_.datatype).exists(hasReferenceToModel(_, datatype :: visited))
            }
          case utype: Union =>
            findUnionByName(utype.fullName).fold(false) { u =>
              val datatypes = u.types.map(_.datatype)

              if (datatypes.exists {
                case mtype: Model => findModelByName(mtype.fullName).fold(false)(_ == model)
                case _ => false // can a union have another union as a possible type?
              })
                true
              else
                datatypes.exists(hasReferenceToModel(_, datatype :: visited))
            }
          case _ => false
        }
      }
    }

    def getShortName(dt: ScalaDatatype): String = {
      dt match {
        case model: ScalaPrimitive.Model => model.shortName
        case union: ScalaPrimitive.Union => union.shortName
        case ScalaDatatype.Option(inner) => getShortName(inner)
        case ScalaDatatype.List(inner)   => getShortName(inner)
        case ScalaDatatype.Map(inner)    => getShortName(inner)
      }
    }

    val serializations = model.fields.map { field =>
      val beLazy = hasReferenceToModel(field.datatype)

      field.datatype match {
        case ScalaDatatype.Option(inner) => {
          val path = s"""(__ \\ "${field.originalName}")"""
          val reader = {
            if (beLazy)
              s"""lazyReadNullable(${play2JsonCommon.implicitReaderName(getShortName(inner))})"""
            else
              s"""readNullable[${inner.name}]"""
          }

          s"$path.$reader"
        }
        case datatype => {
          val path = s"""(__ \\ "${field.originalName}")"""
          val reader = {
            if (beLazy)
              s"""lazyRead(${play2JsonCommon.implicitReaderName(getShortName(datatype))})"""
            else
              s"""read[${datatype.name}]"""
          }

          s"$path.$reader"
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

    val (optionalFields, requiredFields) = model.fields.partition { f => isOption(f.datatype) }

    val base = Seq(
      Seq(
        s"def $method(obj: ${model.qualifiedName}): play.api.libs.json.JsObject = {",
        Seq(
          nilToOption(requiredFields).map { fields =>
            Seq(
              "play.api.libs.json.Json.obj(",
              fields.map { field =>
                val js = getJsonObject(field.originalName, field.datatype, s"obj.${field.name}").value
                  s""""${field.originalName}" -> $js"""
              }.mkString(",\n").indent(2),
              ")"
            ).mkString("\n")
          },
          nilToOption(optionalFields).map { fields =>
            fields.map { field =>
              getJsonObject(field.originalName, field.datatype, s"obj.${field.name}").obj
            }.mkString("(", ") ++\n(", ")")
          }
        ).flatten.mkString(" ++ ").indent(2),
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

  private[this] def nilToOption[T](values: Seq[T]): Option[Seq[T]] = {
    values match {
      case Nil => None
      case _ => Some(values)
    }
  }

  private[this] def unionTypesWithNames(union: ScalaUnion): Seq[(ScalaUnionType, String)] = {
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

  private[this] def isOption(datatype: ScalaDatatype): Boolean = {
    datatype match {
      case ScalaDatatype.Option(_) => true
      case _ => false
    }
  }


  /**
   * Returns a JSON Object containing only this value
   */
  private[this] def getJsonObject(originalName: String, datatype: ScalaDatatype, varName: String): JsObjectResult = {
    datatype match {
      case ScalaPrimitive.Enum(ns, name) => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString(${varName}.toString)")
      }
      case ScalaPrimitive.Model(ns, name) => {
        toJsObjectResult(originalName, play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)")
      }
      case ScalaPrimitive.Union(ns, name) => {
        toJsObjectResult(originalName, play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)")
      }
      case ScalaPrimitive.Boolean => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsBoolean(${varName})")
      }
      case ScalaPrimitive.Decimal | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsNumber($varName)")
      }
      case ScalaPrimitive.DateIso8601Joda | ScalaPrimitive.DateIso8601Java | ScalaPrimitive.DateTimeIso8601Java | ScalaPrimitive.Uuid => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString(${varName}.toString)")
      }
      case ScalaPrimitive.String => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString($varName)")
      }
      case ScalaPrimitive.DateTimeIso8601Joda => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($varName))")
      }
      case ScalaPrimitive.ObjectAsPlay => {
        toJsObjectResult(originalName, varName)
      }
      case ScalaPrimitive.ObjectAsCirce => {
        toJsObjectResult(originalName, varName)
      }
      case ScalaDatatype.Option(inner) => {
        val value = getJsonObject(originalName, inner, "x").value
        JsObjectResult(
          Seq(
            s"$varName match {",
            "  case None => play.api.libs.json.Json.obj()",
            "  case Some(x) => " + createJsonObject(originalName, value),
            "}"
          ).mkString("\n"),
          value
        )
      }
      case ScalaDatatype.List(_) | ScalaDatatype.Map(_) | ScalaPrimitive.Unit => {
        toJsObjectResult(originalName, s"play.api.libs.json.Json.toJson($varName)")
      }
    }
  }

  private[this] case class JsObjectResult(obj: String, value: String)

  private[this] def toJsObjectResult(name: String, value: String): JsObjectResult = {
    JsObjectResult(
      createJsonObject(name, value),
      value
    )
  }

  private[this] def createJsonObject(name: String, value: String): String = {
    s"""play.api.libs.json.Json.obj("$name" -> $value)"""
  }

  /**
   * Assumes all primitives are wrapped in primitive wrappers
   */
  private[this] def getJsonValueForUnion(
    datatype: ScalaDatatype,
    varName: String,
    discriminator: Option[Discriminator] = None
  ): String = {
    datatype match {
      case ScalaPrimitive.Boolean => {
        wrapInObject(s"play.api.libs.json.JsBoolean(${varName}.value)", discriminator)
      }
      case ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long => {
        wrapInObject(s"play.api.libs.json.JsNumber(${varName}.value)", discriminator)
      }
      case ScalaPrimitive.DateIso8601Joda | ScalaPrimitive.DateIso8601Java | ScalaPrimitive.DateTimeIso8601Java | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => {
        wrapInObject(s"play.api.libs.json.JsString(${varName}.value.toString)", discriminator)
      }
      case ScalaPrimitive.String => {
        wrapInObject(s"play.api.libs.json.JsString(${varName}.value)", discriminator)
      }
      case ScalaPrimitive.DateTimeIso8601Joda => {
        wrapInObject(s"play.api.libs.json.JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(${varName}.value))", discriminator)
      }
      case ScalaPrimitive.ObjectAsPlay | ScalaPrimitive.ObjectAsCirce => {
        wrapInObject(s"play.api.libs.json.Json.obj(${varName}.value)", discriminator)
      }
      case ScalaPrimitive.Enum(ns, name) => {
        discriminator match {
          case None => {
            s"play.api.libs.json.JsString(${varName}.toString)"
          }
          case Some(_) => {
            wrapInObject(s"play.api.libs.json.JsString(${varName}.toString)", discriminator)
          }
        }
      }
      case ScalaPrimitive.Model(ns, name) => {
        mergeDiscriminator(play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)", discriminator)
      }
      case ScalaPrimitive.Union(ns, name) => {
        mergeDiscriminator(play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)", discriminator)
      }
      case ScalaDatatype.List(_) | ScalaDatatype.Map(_) | ScalaDatatype.Option(_) | ScalaPrimitive.Unit => {
        mergeDiscriminator(s"play.api.libs.json.Json.toJson($varName)", discriminator)
      }
    }
  }

  private[this] def wrapInObject(
    value: String,
    discriminator: Option[Discriminator]
  ): String = {
    discriminator match {
      case None => {
        createJsonObject(PrimitiveWrapper.FieldName, value)
      }
      case Some(disc) => {
        s"""play.api.libs.json.Json.obj("${disc.name}" -> "${disc.value}", "${PrimitiveWrapper.FieldName}" -> $value)"""
      }
    }
  }

  private[this] def mergeDiscriminator(
    value: String,
    discriminator: Option[Discriminator]
  ): String = {
    discriminator match {
      case None => {
        value
      }
      case Some(disc) => {
        s"$value ++ " + createJsonObject(disc.name, s""""${disc.value}"""")
      }
    }
  }

  private[this] case class Discriminator(name: String, value: String)

}
