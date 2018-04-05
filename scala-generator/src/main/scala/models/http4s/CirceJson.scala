package scala.models.http4s

import lib.Text._
import scala.generator.{PrimitiveWrapper, ScalaDatatype, ScalaEnum, ScalaModel, ScalaPrimitive, ScalaUnion, ScalaUnionType}
import scala.models.JsonImports

case class CirceJson(
  ssd: ScalaService
) {
  def generate(): String = {
    s"""package ${ssd.namespaces.models} {

  package object json {
    import io.circe.Decoder._
    import io.circe.Encoder._
    import scala.util.Try
    import io.circe.{Json, JsonObject, Encoder, Decoder, DecodingFailure}
    import io.circe.syntax._
${JsonImports(ssd.service).mkString("\n").indent(4)}

    // Make Scala 2.11 Either monadic
    private[${ssd.namespaces.last}] implicit def eitherOps[A,B](e: Either[A,B]) = cats.implicits.catsSyntaxEither(e)

    private[${ssd.namespaces.last}] implicit val decodeUUID: Decoder[_root_.java.util.UUID] =
      Decoder.decodeString.emapTry(str => Try(_root_.java.util.UUID.fromString(str)))

    private[${ssd.namespaces.last}] implicit val encodeUUID: Encoder[_root_.java.util.UUID] =
      Encoder.encodeString.contramap[_root_.java.util.UUID](_.toString)

    private[${ssd.namespaces.last}] implicit val decodeInstant: Decoder[_root_.java.time.Instant] =
      Decoder.decodeString.emapTry(str => Try(_root_.java.time.Instant.parse(str)))

    private[${ssd.namespaces.last}] implicit val encodeInstant: Encoder[_root_.java.time.Instant] =
      Encoder.encodeString.contramap[_root_.java.time.Instant](_.toString)

    private[${ssd.namespaces.last}] implicit val decodeLocalDate: Decoder[_root_.java.time.LocalDate] =
      Decoder.decodeString.emapTry(str => Try(_root_.java.time.LocalDate.parse(str)))

    private[${ssd.namespaces.last}] implicit val encodeLocalDate: Encoder[_root_.java.time.LocalDate] =
      Encoder.encodeString.contramap[_root_.java.time.LocalDate](_.toString)

${Seq(generateEnums(), generateModels(), generateUnions()).filter(!_.isEmpty).mkString("\n\n").indent(4)}
  }
}"""
  }

  def generateModels(): String = {
    Seq(
      ssd.models.map(decodersAndEncoders(_)).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => decodersAndEncoders(w.model)).mkString("\n\n")
    ).filter(!_.trim.isEmpty).mkString("\n\n")
  }

  def generateUnions(): String = {
    ssd.unions.map(decodersAndEncoders(_)).mkString("\n\n")
  }

  /**
    * Returns the implicits for enum json serialization, handling
    * conversion both from the string and object representations.
    */
  def generateEnums(): String = {
    ssd.enums.map(enumDecodersAndEncoders(_)).mkString("\n\n")
  }

  private[models] def enumDecodersAndEncoders(enum: ScalaEnum): String = {
    Seq(
      s"""implicit val jsonDecoder${ssd.name}${enum.name}: Decoder[${enum.qualifiedName}] =""",
      s"""  Decoder.decodeString.map(${enum.qualifiedName}(_))""",
      "",
      s"""implicit val jsonEncoder${ssd.name}${enum.name}: Encoder[${enum.qualifiedName}] =""",
      s"""  Encoder.encodeString.contramap[${enum.qualifiedName}](_.toString)"""
    ).mkString("\n")
  }

  private def decodersAndEncoders(union: ScalaUnion): String = {
    decoders(union) + "\n\n" + encoders(union)
  }

  private[models] def decoders(union: ScalaUnion): String = {
    union.discriminator match {
      case None => decodersWithoutDiscriminator(union)
      case Some(discriminator) => decodersWithDiscriminator(union, discriminator)
    }
  }

  private[this] def decodersWithoutDiscriminator(union: ScalaUnion): String = {
    Seq(
      s"${implicitDecoderDef(union.name)} = Decoder.instance { c =>",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""c.get[$typeName]("${t.discriminatorName}") orElse"""
      }.mkString("\n").indent(2),
      s"  Right(${union.undefinedType.fullName}(c.value.toString))",
      s"}"
    ).mkString("\n")
  }

  private[this] def decodersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    val typesWithNames = unionTypesWithNames(union)
    val defaultClause = typesWithNames.filter(_._1.isDefault).headOption match {
      case Some((_, typeName)) => s"""c.as[$typeName]"""
      case None => s"""Left(DecodingFailure("Union[${union.name}] requires a discriminator named '$discriminator' - this field was not found in the Json", c.history))"""
    }

    Seq(
      s"""${implicitDecoderDef(union.name)} = Decoder.instance { c =>""",
      s"""  c.get[Option[String]]("$discriminator") match {""",
      typesWithNames.map { case (t, typeName) =>
        s"""  case Right(Some(s)) if s == "${t.discriminatorName}" => c.as[$typeName]"""
      }.mkString("\n").indent(2),
      s"""    case Right(Some(s)) => Right(${union.undefinedType.fullName}(s))""",
      s"""    case _ => $defaultClause""",
      s"""  }""",
      s"""}"""
    ).mkString("\n")
  }

  private[models] def encoders(union: ScalaUnion): String = {
    Seq(
      union.discriminator match {
        case None => encodersWithoutDiscriminator(union)
        case Some(discriminator) => encodersWithDiscriminator(union, discriminator)
      }
    ).mkString("\n\n")
  }

  private[models] def encodersWithoutDiscriminator(union: ScalaUnion): String = {
    Seq(
      s"${implicitEncoderDef(union.name)} = Encoder.instance {",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""case t: ${typeName} => Json.fromJsonObject(JsonObject.singleton("${t.discriminatorName}", t.asJson))"""
      }.mkString("\n").indent(2),
      s"""  case other => sys.error(s"The type[$${other.getClass.getName}] has no JSON encoder")""",
      "}"
    ).mkString("\n")
  }

  private[models] def encodersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    Seq(
      s"${implicitEncoderDef(union.name)} = Encoder.instance {",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""case t: ${typeName} => t.asJson.mapObject(obj => ("$discriminator", Json.fromString("${t.discriminatorName}")) +: obj)"""
      }.mkString("\n").indent(2),
      s"""  case other => sys.error(s"The type[$${other.getClass.getName}] has no JSON encoder")""",
      "}"
    ).mkString("\n")
  }

  private[models] def decodersAndEncoders(model: ScalaModel): String = {
    decoders(model) ++ "\n\n" ++ encoders(model)
  }
  
  private[models] def decoders(model: ScalaModel): String = {
    // backticks don't work correctly as enumerator names in for comprehensions
    def nobt(fieldName:String) = fieldName.replaceAll("`", "__")
    Seq(
      s"${implicitDecoderDef(model.name)} = Decoder.instance { c =>",
      s" for {",
      model.fields.map { field =>
        field.datatype match {
          case ScalaDatatype.Option(inner) => {
            s"""${nobt(field.name)} <- c.downField("${field.originalName}").as[Option[${inner.name}]]"""
          }
          case datatype if field.shouldApplyDefaultOnRead => {
            s"""${nobt(field.name)} <- c.downField("${field.originalName}").as[Option[${datatype.name}]]"""
          }
          case datatype => {
            s"""${nobt(field.name)} <- c.downField("${field.originalName}").as[${datatype.name}]"""
          }
        }
      }.mkString("\n").indent(4),
      s"  } yield {",
      s"    val ret = ${model.name}(",

      model
        .fields
        .filterNot(_.shouldApplyDefaultOnRead)
        .map {field =>
        s"""${nobt(field.name)} = ${field.name}"""
      }.mkString(",\n").indent(6),

      s"    )",
      s"    Some(ret)",
      model.fields
        .filter(f=> f.shouldApplyDefaultOnRead)
        .map{ field =>
          s""".map(m=> ${nobt(field.name)}.map(d=> m.copy(${field.name}=d)).getOrElse(m))"""
        }.mkString("\n").indent(6),
      s"      .get",
      s"  }",
      s"}"
    ).mkString("\n")
  }

  private[models] def encoders(model: ScalaModel): String = {
    Seq(
      s"${implicitEncoderDef(model.name)} = Encoder.instance { t =>",
      s"  Json.fromFields(Seq(",
      model.fields.map { field =>
        if (field.shouldModelConcreteType) {
          s"""t.${field.name}.map(t => "${field.originalName}" -> t.asJson)"""
        } else {
          s"""Some("${field.originalName}" -> t.${field.name}.asJson)"""
        }
      }.mkString(",\n").indent(4),
      s"  ).flatten)",
      "}"
    ).mkString("\n")
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

  private[this] def implicitDecoderDef(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"implicit def decode${ssd.name}$name: Decoder[$name]"
  }

  private[this] def implicitEncoderDef(name: String): String = {
    assert(name.indexOf(".") < 0, s"Invalid name[$name]")
    s"implicit def encode${ssd.name}$name: Encoder[$name]"
  }
}
