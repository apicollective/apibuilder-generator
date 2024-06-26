package scala.models

import lib.Text._

import scala.annotation.nowarn
import scala.generator.ScalaPrimitive.{Model, Union}
import scala.generator.{ScalaUnionType, _}

case class Play2JsonCommon(ssd: ScalaService, scala3Support: Boolean) {

  /**
    * Never use the service name in the method name. We never import
    * this method implicitly and we need to be able to reference
    * method names defined for other services.
    */
  def toJsonObjectMethodName(ns: Namespaces, name: String): String = {
    val method = s"jsObject$name"
    if (ns.base == ssd.namespaces.base) {
      method
    } else {
      Seq(ns.json, method).mkString(".")
    }
  }

  private[models] def implicitWriter(name: String, qualifiedName: String, methodName: String, model: Option[ScalaModel]): String = {
    Seq(
      s"${implicitWriterDef(name, model = model)} = {",
      s"  (obj: $qualifiedName) => {",
      s"    ${methodName}(obj)",
      "  }",
      "}"
    ).mkString("\n")
  }

  private[models] def implicitUnionWriter(union: ScalaUnion): String = {
    val name = implicitWriterName(union.name)
    val methodName = toJsonObjectMethodName(ssd.namespaces, union.name)
    if (scala3Support) {
      s"""
         |implicit def $name[T <: ${union.qualifiedName}]: play.api.libs.json.Writes[T] = {
         |  (obj: ${union.qualifiedName}) => {
         |    $methodName(obj)
         |  }
         |}
         |""".stripMargin
    } else {
      implicitWriter(union.name, union.qualifiedName, methodName, model = None)
    }
  }

  private[models] def scala3CollectionReaders(union: ScalaUnion): Option[String] = {
    if (scala3Support) {
      val readerName = implicitReaderName(union.name)
      val name = readerName + "Seq"
      Some(s"""
        |implicit def $name[T <: ${union.qualifiedName}]: play.api.libs.json.Reads[Seq[T]] = {
        |  case a: play.api.libs.json.JsArray => {
        |    val all: Seq[play.api.libs.json.JsResult[${union.qualifiedName}]] = a.value.map(${readerName}.reads).toSeq
        |
        |    all.collect { case e: play.api.libs.json.JsError => e }.toList match {
        |      case Nil => play.api.libs.json.JsSuccess(all.collect { case play.api.libs.json.JsSuccess(v, _) => v.asInstanceOf[T] })
        |      case errors => play.api.libs.json.JsError(play.api.libs.json.JsError.merge(errors.flatMap(_.errors), Nil))
        |    }
        |  }
        |  case other => play.api.libs.json.JsError(s"Expected array but found [" + other.getClass.getName + "]")
        |}
        |""".stripMargin)
    } else {
      None
    }
  }

  private[models] def implicitUnionReader(union: ScalaUnion): String = {
    if (scala3Support) {
      s"implicit def ${implicitReaderName(union.name)}[T <: ${union.qualifiedName}]: play.api.libs.json.Reads[T]"
    } else {
      implicitReaderDefStatic(union.name, qualifiedName = union.qualifiedName, model = None)
    }
  }

  private[models] def implicitModelReader(model: ScalaModel): String = {
    implicitReaderDefStatic(model.name, qualifiedName=  model.qualifiedName, model = Some(model))
  }

  private[this] def implicitReaderDefStatic(name: String, qualifiedName: String, model: Option[ScalaModel]): String = {
    s"${maybeImplicit(model)}def ${implicitReaderName(name)}: play.api.libs.json.Reads[$qualifiedName]"
  }

  /**
   * Scala3 changed the inference model which results in some cases multiple implicits
   * matching when there are multiple reads/writes for a Model and its parent Union.
   *
   * For scala3, we remove implicit readers/writes for models that are part of a union type -
   * they must be read/written with the parent type.
   */
  private[this] def maybeImplicit(model: Option[ScalaModel]): String = {
    model match {
      case Some(m) if scala3Support && partOfUnion(m) => ""
      case _ => "implicit "
    }
  }

  private[this] def partOfUnion(model: ScalaModel): Boolean = {
    ssd.unionsForModel(model).nonEmpty
  }

  private[models] def implicitReaderName(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"jsonReads${ssd.name}$name"
  }

  private[models] def implicitWriterName(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"jsonWrites${ssd.name}$name"
  }

  private[this] def implicitWriterDef(name: String, model: Option[ScalaModel])(implicit typeDecl: String = name): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    val methodName = implicitWriterName(name)
    s"${maybeImplicit(model)}def $methodName: play.api.libs.json.Writes[$typeDecl]"
  }

}

case class Play2Json(
  ssd: ScalaService, scala3Support: Boolean
) {

  private[this] val play2JsonCommon = Play2JsonCommon(ssd, scala3Support = scala3Support)

  def generateModelsAndUnions(): String = {
    Seq(
      ssd.models.map(readersAndWriters).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => readers(w.model)).mkString("\n\n"),
      ssd.unions.map(readersAndWriters).mkString("\n\n")
    ).filter(_.trim.nonEmpty).mkString("\n\n")
  }

  /**
    * Returns the implicits for enum json serialization, handling
    * conversion both from the string and object representations.
    */
  def generateEnums(): String = {
    ssd.enums.map(enumReadersAndWriters).mkString("\n\n")
  }

  private[models] def enumReadersAndWriters(`enum`: ScalaEnum): String = {
    val jsValueWriterMethod = play2JsonCommon.implicitWriterName(enum.name)
    val jsObjectWriterMethod = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, enum.name)

    val discriminator = getDiscriminator(enum)
    val jsObjectBody = wrapInObject("play.api.libs.json.JsString(obj.toString)", discriminator)

    // if there is a discriminator, writes using the object version, otherwise use the value
    val implicitWriter = discriminator match {
      case Some(_) =>
        play2JsonCommon.implicitWriter(enum.name, enum.qualifiedName, jsObjectWriterMethod, model = None)
      case None =>
        play2JsonCommon.implicitWriter(enum.name, enum.qualifiedName, jsValueWriterMethod, model = None)
    }

    Seq(
      s"implicit val jsonReads${ssd.name}${enum.name}: play.api.libs.json.Reads[${enum.qualifiedName}] = new play.api.libs.json.Reads[${enum.qualifiedName}] {",
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
                "case err: play.api.libs.json.JsError =>",
                Seq(
                  s"""(js \\ "${enum.originalName}").validate[String] match {""",
                  Seq(
                    s"case play.api.libs.json.JsSuccess(v, _) => play.api.libs.json.JsSuccess(${enum.qualifiedName}(v))",
                    "case err: play.api.libs.json.JsError => err",
                  ).mkString("\n").indentString(2),
                  "}"
                ).mkString("\n").indentString(2),
              ).mkString("\n").indentString(2),
              "}"
            ).mkString("\n").indentString(2),
            "}"
          ).mkString("\n").indentString(2),
          "}"
        ).mkString("\n").indentString(2),
        "}"
      ).mkString("\n").indentString(2),
      "}",
      "",
      s"def $jsValueWriterMethod(obj: ${enum.qualifiedName}) = {",
      s"""  play.api.libs.json.JsString(obj.toString)""",
      s"}",
      "",
      s"def $jsObjectWriterMethod(obj: ${enum.qualifiedName}) = {",
      s"""  $jsObjectBody""",
      s"}",
      "",
      implicitWriter
    ).mkString("\n")
  }

  private def readersAndWriters(union: ScalaUnion): String = {
    readers(union) + "\n\n" + writers(union)
  }

  private[models] def readers(union: ScalaUnion): String = {
    (
      Seq(
          union.discriminator match {
            case None => readersWithoutDiscriminator(union)
            case Some(discriminator) => readersWithDiscriminator(union, discriminator)
          }
        ) ++ play2JsonCommon.scala3CollectionReaders(union).toSeq
      ).mkString("\n\n").trim
  }

  private[this] def readersWithoutDiscriminator(union: ScalaUnion): String = {
    Seq(
      s"${play2JsonCommon.implicitUnionReader(union)} = {",
      s"  (",
      union.types.map { scalaUnionType =>
        s"""(__ \\ "${scalaUnionType.discriminatorName}").read(${reader(union, scalaUnionType)}).asInstanceOf[play.api.libs.json.Reads[${union.name}]]"""
      }.mkString("\norElse\n").indentString(4),
      s"    orElse",
      s"    play.api.libs.json.Reads(jsValue => play.api.libs.json.JsSuccess(${union.undefinedType.model.name}(jsValue.toString))).asInstanceOf[play.api.libs.json.Reads[${union.name}]]",
      s"  )",
      s"}"
    ).mkString("\n")
  }

  private[this] def readersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    val scala3Cast = if (scala3Support) {
      ".map(_.asInstanceOf[T])"
    } else {
      ""
    }
    val defaultDiscriminatorClause = union.defaultType match {
      case None => "case e: play.api.libs.json.JsError => e"
      case Some(defaultType) => s"case _: play.api.libs.json.JsError => readDiscriminator(\"${defaultType.discriminatorName}\")"
    }

    Seq(
      s"${play2JsonCommon.implicitUnionReader(union)} = (js: play.api.libs.json.JsValue) => {",
      Seq(
        "def readDiscriminator(discriminator: String) = {",
        Seq(
          "discriminator match {",
          unionTypesWithNames(union).map { t =>
            // OLD: Pre scala3: s"""case "${t.unionType.discriminatorName}" => js.validate[${t.typeName}]"""
            s"""case "${t.unionType.discriminatorName}" => ${play2JsonCommon.implicitReaderName(t.unionType.name)}.reads(js)"""
          }.mkString("\n").indentString(2),
          s"""case other => play.api.libs.json.JsSuccess(${union.undefinedType.datatype.fullName}(other))""".indentString(2),
          "}"
        ).mkString("\n").indentString(2),
        "}"
      ).mkString("\n").indentString(2),
      Seq(s"""(js \\ "$discriminator").validate[String] match {""",
        Seq(
          defaultDiscriminatorClause,
          s"case s: play.api.libs.json.JsSuccess[String] => readDiscriminator(s.value)$scala3Cast",
        ).mkString("\n").indentString(2),
        "}"
      ).mkString("\n").indentString(2),
      "}",
    ).mkString("\n")
  }

  private[models] def writers(union: ScalaUnion): String = {
    Seq(
      union.discriminator match {
        case None => writersWithoutDiscriminator(union)
        case Some(discriminator) => writersWithDiscriminator(union, discriminator)
      },
      play2JsonCommon.implicitUnionWriter(union)
    ).mkString("\n")
  }

  private[models] def writersWithoutDiscriminator(union: ScalaUnion): String = {
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, union.name)

    Seq(
      s"def $method(obj: ${union.qualifiedName}): play.api.libs.json.JsObject = {",
      s"  obj match {",
      unionTypesWithNames(union).map { t =>
        val json = getJsonValueForUnion(t.unionType.datatype, "x")
        s"""case x: ${t.typeName} => play.api.libs.json.Json.obj("${t.unionType.discriminatorName}" -> $json)"""
      }.mkString("\n").indentString(4),
      s"""    case x: ${union.undefinedType.datatype.fullName} => sys.error(s"The type[${union.undefinedType.datatype.fullName}] should never be serialized")""",
      "  }",
      "}"
    ).mkString("\n")
  }

  @nowarn("msg=possible missing interpolator")
  private[models] def writersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, union.name)

    Seq(
      s"def $method(obj: ${union.qualifiedName}): play.api.libs.json.JsObject = {",
      Seq(
        "obj match {",
        Seq(
          unionTypesWithNames(union).map { t =>
            val json = getJsonValueForUnion(t.unionType.datatype, "x", Some(Discriminator(discriminator, t.unionType.discriminatorName)))
            s"case x: ${t.typeName} => $json"
          }.mkString("\n"),
          s"case other => {",
          """  sys.error(s"The type[${other.getClass.getName}] has no JSON writer")""",
          "}"
        ).mkString("\n").indentString(2),
        "}"
      ).mkString("\n").indentString(2),
      "}"
    ).mkString("\n")
  }

  private def reader(union: ScalaUnion, ut: ScalaUnionType): String = {
    ut.model match {
      case Some(_) => {
        play2JsonCommon.implicitReaderName(ut.name)
      }
      case None => {
        ut.enum match {
          case Some(_) => {
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
              case dt => sys.error(s"unsupported datatype[$dt] in union $ut")
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
      s"${play2JsonCommon.implicitModelReader(model)} = {",
      fieldReaders(model).indentString(2),
      s"}"
    ).mkString("\n")
  }

  private[this] def findModelByName(name: String): Option[ScalaModel] =
    ssd.models.find(_.qualifiedName == name)

  private[this] def findUnionByName(name: String): Option[ScalaUnion] =
    ssd.unions.find(_.qualifiedName == name)

  private[models] def fieldReaders(model: ScalaModel): String = {

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

    val serializations = model.fields.map { field =>
      val beLazy = hasReferenceToModel(field.datatype)
      val path = s"""(__ \\ "${field.originalName}")"""

      val reader = (field.datatype, field.shouldApplyDefaultOnRead, beLazy) match {
        case (ScalaDatatype.Option(inner), true, true)   => s"""lazyReadNullable(play.api.libs.json.Reads.withDefault[${inner.name}](${field.default.get})"""
        case (ScalaDatatype.Option(inner), true, false)  => s"""readNullableWithDefault[${inner.name}](${field.default.get})"""
        case (ScalaDatatype.Option(inner), false, true)  => s"""lazyReadNullable(play.api.libs.json.Reads.of[${inner.name}])"""
        case (ScalaDatatype.Option(inner), false, false) => s"""readNullable[${inner.name}]"""
        case (datatype, true, true)                      => s"""lazyRead(play.api.libs.json.Reads.withDefault[${datatype.name}](${field.default.get})"""
        case (datatype, true, false)                     => s"""readWithDefault[${datatype.name}](${field.default.get})"""
        case (datatype, false, true)                     => s"""lazyRead(play.api.libs.json.Reads.of[${datatype.name}])"""
        case (datatype, false, false)                    => s"""read[${datatype.name}]"""
      }
      s"$path.$reader"
    }

    model.fields match {
      case field :: Nil =>
        serializations.head + s""".map { x => ${model.name}(${field.name} = x) }"""

      case fields =>
        val constructorCall = s"${model.name}(${fields.map(_.name).mkString(", ")})"

        val forComprehensions = (fields zip serializations).map { case (field, reader) =>
          s"${field.name} <- $reader".indentString(2)
        }

        s"""for {
           |${forComprehensions.mkString("\n")}
           |} yield $constructorCall""".stripMargin
    }
  }

  private[models] def writers(model: ScalaModel): String = {
    val method = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, model.name)

    val (optionalFields, requiredFields) = model.fields.partition { f => isOption(f.datatype) }

    val discriminatorOpt = getDiscriminator(model)

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
              }.mkString(",\n").indentString(2),
              ")"
            ).mkString("\n")
          },
          nilToOption(optionalFields).map { fields =>
            fields.map { field =>
              getJsonObject(field.originalName, field.datatype, s"obj.${field.name}").obj
            }.mkString("(", ") ++\n(", ")")
          },
          discriminatorOpt.map(createJsonObject)
        ).flatten.mkString(" ++ ").indentString(2),
        "}"
      ).mkString("\n")
    ).mkString("\n\n")

    Seq(
      base,
      play2JsonCommon.implicitWriter(model.name, model.qualifiedName, method, model = Some(model))
    ).mkString("\n\n")
  }

  private[this] def getDiscriminator(model: ScalaModel): Option[Discriminator] =
    getDiscriminator("Model", model.qualifiedName, ssd.unionAndTypesForModel(model))

  private[this] def getDiscriminator(`enum`: ScalaEnum): Option[Discriminator] =
    getDiscriminator("Enum", enum.qualifiedName, ssd.unionsAndTypesForEnum(enum))

  private[this] def getDiscriminator(
    name: String,
    modelType: String,
    unionAndTypes: Seq[(ScalaUnion, ScalaUnionType)]
  ): Option[Discriminator] = {
    val discriminatorNameValues =
      unionAndTypes
      .map { case (union, unionType) => (union.discriminator, unionType.discriminatorName) }
      .distinct

    if (discriminatorNameValues.size > 1) {
      val nvs = discriminatorNameValues.map { case (n, v) => (n.getOrElse("''"), v) }.mkString(", ")
      sys.error(s"$modelType $name must have a unique discriminator name and values. Found: $nvs")
    } else
      discriminatorNameValues.headOption.flatMap { case (name, v) => name.map(Discriminator(_, v)) }
  }

  private[this] def nilToOption[T](values: Seq[T]): Option[Seq[T]] = {
    values match {
      case Nil => None
      case _ => Some(values)
    }
  }

  private[this] case class UnionTypeWithName(unionType: ScalaUnionType, typeName: String)
  private[this] def unionTypesWithNames(union: ScalaUnion): Seq[UnionTypeWithName] = {
    union.types.flatMap { t =>
      def single(name: String) = Seq(UnionTypeWithName(t, name))
      t.datatype match {
        case p @ (ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _)) => {
          single(p.name)
        }
        case p: ScalaPrimitive.Union => {
          findUnionByName(p.fullName) match {
            case None => single(p.name)
            case Some(u) => unionTypesWithNames(u)
          }
        }
        case p: ScalaPrimitive => single(ssd.modelClassName(PrimitiveWrapper.className(union, p)))
        case c: ScalaDatatype.Container => sys.error(s"unsupported container type $c encountered in union ${union.name}")
      }
    }.distinct
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
      case ScalaPrimitive.Enum(_, _) => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString($varName.toString)")
      }
      case ScalaPrimitive.GeneratedModel(name) => {
        sys.error(s"Cannot convert generated model named '$name' to JSON")
      }
      case ScalaPrimitive.Model(ns, name) => {
        toJsObjectResult(originalName, play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)")
      }
      case ScalaPrimitive.Union(ns, name) => {
        toJsObjectResult(originalName, play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)")
      }
      case ScalaPrimitive.Boolean => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsBoolean($varName)")
      }
      case ScalaPrimitive.Decimal | ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsNumber($varName)")
      }
      case dt @ (_: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Uuid) => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString(${dt.asString(varName)})")
      }
      case ScalaPrimitive.String => {
        toJsObjectResult(originalName, s"play.api.libs.json.JsString($varName)")
      }
      case _ @ (_: ScalaPrimitive.JsonObject | _: ScalaPrimitive.JsonValue)=> {
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

  private[this] def createJsonObject(discriminator: Discriminator): String =
    createJsonObject(discriminator.name, s""""${discriminator.value}"""")

  private[this] def createJsonObject(name: String, value: String): String =
    s"""play.api.libs.json.Json.obj("$name" -> $value)"""

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
        wrapInObject(s"play.api.libs.json.JsBoolean($varName.value)", discriminator)
      }
      case ScalaPrimitive.Double | ScalaPrimitive.Integer | ScalaPrimitive.Long => {
        wrapInObject(s"play.api.libs.json.JsNumber($varName.value)", discriminator)
      }
      case dt @ (_: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid) => {
        wrapInObject(s"play.api.libs.json.JsString(${dt.asString(s"$varName.value")})", discriminator)
      }
      case ScalaPrimitive.String => {
        wrapInObject(s"play.api.libs.json.JsString($varName.value)", discriminator)
      }
      case _: ScalaPrimitive.JsonObject => {
        wrapInObject(s"play.api.libs.json.Json.obj($varName.value)", discriminator)
      }
      case _: ScalaPrimitive.JsonValue => {
        wrapInObject(s"$varName.value)", discriminator)
      }
      case ScalaPrimitive.Enum(ns, name) => {
        discriminator match {
          case None =>
            s"play.api.libs.json.JsString($varName.toString)"
          case Some(_) =>
            play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)"
        }
      }
      case ScalaPrimitive.GeneratedModel(name) => {
        sys.error(s"Cannot convert generated model named '$name' from JSON")
      }
      case ScalaPrimitive.Model(ns, name) => {
        play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)"
      }
      case ScalaPrimitive.Union(ns, name) => {
        play2JsonCommon.toJsonObjectMethodName(ns, name) + s"($varName)"
      }
      case ScalaDatatype.List(_) | ScalaDatatype.Map(_) | ScalaDatatype.Option(_) | ScalaPrimitive.Unit => {
        s"play.api.libs.json.Json.toJson($varName)"
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

  private[this] case class Discriminator(name: String, value: String)

}
