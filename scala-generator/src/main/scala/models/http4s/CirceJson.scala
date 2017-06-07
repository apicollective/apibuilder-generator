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
    import io.circe.{Json, JsonObject, Encoder, Decoder, DecodingFailure}
    import io.circe.syntax._
${JsonImports(ssd.service).mkString("\n").indent(4)}

    private[${ssd.namespaces.last}] implicit val decodeUUID =
      Decoder.decodeString.map(java.util.UUID.fromString)

    private[${ssd.namespaces.last}] implicit val encodeUUID =
      Encoder.encodeString.contramap[java.util.UUID](_.toString)

    private[${ssd.namespaces.last}] implicit val decodeJodaDateTime =
      Decoder.decodeString.map(org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime)

    private[${ssd.namespaces.last}] implicit val encodeJodaDateTime =
      Encoder.encodeString.contramap[org.joda.time.DateTime](org.joda.time.format.ISODateTimeFormat.dateTime.print)

${Seq(generateEnums(), generateModels(), generateUnions()).filter(!_.isEmpty).mkString("\n\n").indent(4)}
  }
}"""
  }

  def generateModels(): String = {
    Seq(
      ssd.models.map(decodersAndEncoders(_)).mkString("\n\n"),
      PrimitiveWrapper(ssd).wrappers.map(w => decoders(w.model)).mkString("\n\n")
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
      s"""implicit val jsonDecoder${ssd.name}${enum.name}: Decoder[${enum.qualifiedName}] = """,
      s"""  Decoder.decodeString.map(${enum.qualifiedName}(_))""",
      "",
      s"""implicit val jsonEncoder${ssd.name}${enum.name}: Encoder[${enum.qualifiedName}] = """,
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
      s"  import cats.implicits._",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""c.get[$typeName]("${t.originalName}") orElse"""
      }.mkString("\n").indent(2),
      s"  Right(${union.undefinedType.fullName}(c.value.toString))",
      s"}"
    ).mkString("\n")
  }

  private[this] def decodersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    Seq(
      s"""${implicitDecoderDef(union.name)} = Decoder.instance { c =>""",
      s"""  c.get[String]("$discriminator") match {""",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""  case Right(s) if s == "${t.originalName}" => c.as[$typeName]"""
      }.mkString("\n").indent(2),
      s"""    case Right(s) => Right(${union.undefinedType.fullName}(s))""",
      s"""    case _ => Left(DecodingFailure("Attempt to decode value on failed cursor", c.history))""",
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
        s"""case t: ${typeName} => Json.fromJsonObject(JsonObject.singleton("${t.originalName}", t.asJson))"""
      }.mkString("\n").indent(2),
      s"""  case other => sys.error(s"The type[$${other.getClass.getName}] has no JSON encoder")""",
      "}"
    ).mkString("\n")
  }

  private[models] def encodersWithDiscriminator(union: ScalaUnion, discriminator: String): String = {
    Seq(
      s"${implicitEncoderDef(union.name)} = Encoder.instance {",
      unionTypesWithNames(union).map { case (t, typeName) =>
        s"""case t: ${typeName} => t.asJson.mapObject(obj => ("$discriminator", Json.fromString("${t.originalName}")) +: obj)"""
      }.mkString("\n").indent(2),
      s"""  case other => sys.error(s"The type[$${other.getClass.getName}] has no JSON encoder")""",
      "}"
    ).mkString("\n")
  }

  private[models] def decodersAndEncoders(model: ScalaModel): String = {
    decoders(model) ++ "\n\n" ++ encoders(model)
  }
  private[models] def decoders(model: ScalaModel): String = {
    Seq(
      s"${implicitDecoderDef(model.name)} = Decoder.instance { c =>",
      s" for {",
      model.fields.map { field =>
        field.datatype match {
          case ScalaDatatype.Option(inner) => {
            s"""${field.name} <- c.downField("${field.originalName}").as[Option[${inner.name}]]"""
          }
          case datatype => {
            s"""${field.name} <- c.downField("${field.originalName}").as[${datatype.name}]"""
          }
        }
      }.mkString("\n").indent(4),
      s"  } yield {",
      s"    ${model.name}(",
      model.fields.map { field =>
        s"""${field.name} = ${field.name}"""
      }.mkString(",\n").indent(6),
      s"    )",
      s"  }",
      s"}"
    ).mkString("\n")
  }

  private[models] def encoders(model: ScalaModel): String = {
    Seq(
      s"${implicitEncoderDef(model.name)} = Encoder.instance { t =>",
      s"  Json.obj(",
      model.fields.map { field =>
        s"""("${field.originalName}", t.${field.name}.asJson)"""
      }.mkString(",\n").indent(4),
      s"  )",
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
