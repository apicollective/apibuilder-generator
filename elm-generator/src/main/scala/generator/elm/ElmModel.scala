package generator.elm

import io.apibuilder.spec.v0.models.{Field, Model}
import lib.Datatype
import lib.Datatype.{Container, Generated, Primitive, UserDefined}

import scala.util.{Failure, Success}

case class ElmModel(args: GenArgs) {
  private[this] val elmJson = ElmJson(args.imports)
  private[this] val elmType = ElmType(args)

  def generate(model: Model): String = {
    Seq(
      genTypeAlias(model),
      genEncoder(model),
      genDecoder(model)
    ).mkString("\n\n")
  }

  private[this] def genTypeAlias(model: Model): String = {
    Seq(
      s"type alias ${Names.pascalCase(model.name)} =",
      "  {",
      model.fields.map { f =>
        val opt = if (f.required) {
          ""
        } else {
          "Maybe "
        }
        s"${Names.camelCase(f.name)}: $opt${elmType.lookup(f.`type`)}"
      }.mkString("\n, ").indent(4).stripTrailing(),
      "  }"
    ).mkString("\n")
  }


  private[this] def genEncoder(m: Model): String = {
    args.imports.addAs("Json.Encode", "Encode")
    elmJson.encoder(m.name) {
      Seq(
        "Encode.object",
        "[",
        m.fields.zipWithIndex.map { case (f,i) =>
          val encoder = args.datatypeResolver.parse(f.`type`) match {
            case Success(v) => v match {
              case p: Datatype.Primitive => fieldEncoder(f, primitiveEncoder(p))
              case u: UserDefined => {
                fieldDecoder(f, Names.camelCase(args.imports, u.name) + "Encoder")
              }
              case m: Generated.Model => {
                fieldEncoder(f, Names.camelCase(m.name) + "Encoder")
              }
              case u: Container.List => {
                todo(s"model ${m.name} Field ${f.name} has type list: ${u.name}")
              }
              case u: Container.Option => {
                todo(s"model ${m.name} Field ${f.name} has type option: ${u.name}")
              }
              case u: Container.Map => {
                todo(s"model ${m.name} Field ${f.name} has type map: ${u.name}")
              }
            }
            case Failure(ex) => throw ex
          }
          if (i == 0) {
            encoder
          } else {
            s", " + encoder
          }
        }.mkString("\n").indent(4),
        "]"
      ).mkString("\n").indent(4)
    }
  }

  private[this] def primitiveEncoder(p: Primitive): String = {
    args.imports.addAs("Json.Encode", "Encode")
    import Datatype.Primitive._
    p match {
      case Boolean => "Encode.boolean"
      case Double => "Encode.float"
      case Integer => "Encode.int"
      case Long => "Encode.int"
      case DateIso8601 => "Encode.string"
      case DateTimeIso8601 => "Encode.string"
      case Decimal => "Encode.float"
      case Object => "Encode.string" // TODO
      case JsonValue => "Encode.string" // TODO
      case String => "Encode.string"
      case Unit => "(Encode.success Nothing)" // TODO Verify
      case Uuid => "Encode.string"
    }
  }

  private[this] def fieldEncoder(f: Field, encoder: String): String = {
    if (f.required) {
      s"""( ${Names.wrapInQuotes(f.name)}, $encoder instance.${Names.camelCase(f.name)} )"""
    } else {
      s"""( ${Names.wrapInQuotes(f.name)}, encodeOptional $encoder instance.${Names.camelCase(f.name)} )"""
    }
  }

  private[this] def genDecoder(m: Model): String = {
    args.imports.addAs("Json.Decode", "Decode")
    args.imports.addAs("Json.Decode.Pipeline", "Pipeline")

    elmJson.decoder(m.name) {
      Seq(
        s"Decode.succeed ${Names.pascalCase(m.name)}",
        m.fields.zipWithIndex.map { case (f, i) =>
          val decoder = args.datatypeResolver.parse(f.`type`) match {
            case Success(v) => v match {
              case p: Datatype.Primitive => fieldDecoder(f, primitiveEncoder(p))
              case u: UserDefined => {
                fieldDecoder(f, Names.camelCase(args.imports, u.name) + "Decoder")
              }
              case m: Generated.Model => {
                fieldDecoder(f, Names.camelCase(m.name) + "Decoder")
              }
              case u: Container.List => {
                todo(s"model ${m.name} Field ${f.name} has type list: ${u.name}")
              }
              case u: Container.Option => {
                todo(s"model ${m.name} Field ${f.name} has type option: ${u.name}")
              }
              case u: Container.Map => {
                todo(s"model ${m.name} Field ${f.name} has type map: ${u.name}")
              }
            }
            case Failure(ex) => throw ex
          }
          if (i == 0) {
            decoder
          } else {
            s", " + decoder
          }
        }.mkString("\n").indent(4),
        "]"
      ).mkString("\n").indent(4)
    }
  }

  private[this] def fieldDecoder(f: Field, decoder: String): String = {
    s"""|> Pipeline.required ${Names.wrapInQuotes(f.name)} $decoder"""
  }

  private[this] def todo(msg: String): Unit = {
    sys.error(s"TODO: $msg")
  }
}
