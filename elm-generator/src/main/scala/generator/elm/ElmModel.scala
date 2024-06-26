package generator.elm

import cats.implicits._
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models.{Field, Model}
import lib.Datatype
import lib.Datatype.{Container, Generated, Primitive, UserDefined}

import scala.util.{Failure, Success, Try}

case class ElmModel(args: GenArgs) {
  private val elmJson = ElmJson(args.imports)
  private val elmType = ElmTypeLookup(args)

  def generate(model: Model): ValidatedNec[String, String] = {
    (
      wrapErrors { genEncoder(model) },
      wrapErrors { genDecoder(model) }
    ).mapN { case (a, b) => (a, b) }.map { case (encoder, decoder) =>
      Seq(
        genTypeAlias(model),
        encoder.code,
        decoder.code
      ).mkString("\n\n")
    }
  }

  // TODO: Refactor to avoid sys.error
  private def wrapErrors[T](f: => T): ValidatedNec[String, T] = {
    Try {
      f
    } match {
      case Failure(ex) => ex.getMessage.invalidNec
      case Success(r) => r.validNec
    }
  }

  private def genTypeAlias(model: Model): String = {
    Seq(
      s"type alias ${Names.pascalCase(model.name)} =",
      "  {",
      model.fields.map { f =>
        s"${Names.camelCase(f.name)}: ${elmType.lookup(f.`type`, required = f.required).declaration}"
      }.mkString("\n, ").indent(4).stripTrailing(),
      "  }"
    ).mkString("\n")
  }

  private def genEncoderForDatatype(m: Model, f: Field, v: Datatype): String = {
    v match {
      case p: Datatype.Primitive => primitiveEncoder(p)
      case u: UserDefined => {
        Names.camelCase(args.imports, u.name) + "Encoder"
      }
      case m: Generated.Model => {
        Names.camelCase(args.imports, m.name) + "Encoder"
      }
      case u: Container.List => {
        args.imports.addAs("Json.Encode", "Encode")
        s"(Encode.list ${genEncoderForDatatype(m, f, u.inner)})"
      }
      case u: Container.Option => {
        todo(s"model ${m.name} Field ${f.name} has type option: ${u.name}")
      }
      case u: Container.Map => {
        args.imports.addAs("Json.Encode", "Encode")
        args.imports.addExposing("Dict", "Dict")
        args.functions.add(
          """
            |mapEncoder : (a -> Encode.Value) -> Dict String a -> Encode.Value
            |mapEncoder valueEncoder dict =
            |    Encode.object (Dict.toList dict |> List.map (\( k, v ) -> ( k, valueEncoder v )))
            |""".stripMargin
        )
        "(" + Util.maybeWrapInParens("mapEncoder", genEncoderForDatatype(m, f, u.inner)) + ")"
      }
    }
  }


  private def genEncoder(m: Model): ElmFunction = {
    args.imports.addAs("Json.Encode", "Encode")
    elmJson.encoder(m.name) {
      Seq(
        "Encode.object",
        "[",
        m.fields.zipWithIndex.map { case (f, i) =>
          val encoder = args.datatypeResolver.parse(f.`type`) match {
            case Success(v) => {
              val encoder = genEncoderForDatatype(m, f, v)
              if (f.required) {
                s"""( ${Names.wrapInQuotes(f.name)}, $encoder instance.${Names.camelCase(f.name)} )"""
              } else {
                s"""( ${Names.wrapInQuotes(f.name)}, encodeOptional $encoder instance.${Names.camelCase(f.name)} )"""
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

  private def primitiveEncoder(p: Primitive): String = {
    args.imports.addAs("Json.Encode", "Encode")
    import Datatype.Primitive._
    p match {
      case Boolean => "Encode.bool"
      case Double => "Encode.float"
      case Integer => "Encode.int"
      case Long => "Encode.int"
      case DateIso8601 => {
        args.imports.addAs("Util.DateFormatter", "DateFormatter")
        "DateFormatter.encode"
      }
      case DateTimeIso8601 => {
        args.imports.addAs("Iso8601", "Iso8601")
        "Iso8601.encode"
      }
      case Decimal => "Encode.float"
      case Object => "Encode.object"
      case JsonValue => "Encode.object"
      case String => "Encode.string"
      case Unit => "(Encode.success Nothing)" // TODO Verify
      case Uuid => "Encode.string"
    }
  }

  private def genDecoder(m: Model): ElmFunction = {
    elmJson.decoder(m.name) {
      Seq(
        s"Decode.succeed ${Names.pascalCase(m.name)}",
        m.fields.map { f =>
          val decoder = modelFieldDecoder(m, f)
          pipelineDecoder(f, decoder)
        }.mkString("\n").indent(4)
      ).mkString("\n").indent(4)
    }
  }

  private def pipelineDecoder(f: Field, decoder: String): String = {
    args.imports.addAs("Json.Decode.Pipeline", "Pipeline")

    if (f.required) {
      s"""|> Pipeline.required ${Names.wrapInQuotes(f.name)} $decoder"""
    } else {
      args.imports.addAs("Json.Decode", "Decode")
      val nullDecoder = Util.maybeWrapInParens("Decode.nullable", decoder)
      s"""|> Pipeline.optional ${Names.wrapInQuotes(f.name)} ($nullDecoder) Nothing"""
    }
  }

  private def modelFieldDecoder(m: Model, f: Field): String = {
    args.datatypeResolver.parse(f.`type`) match {
      case Success(v) => genDecoderForDatatype(m, f, v)
      case Failure(ex) => throw ex
    }
  }

  private def genDecoderForDatatype(m: Model, f: Field, v: Datatype): String = {
    v match {
      case p: Datatype.Primitive => primitiveDecoder(p)
      case u: UserDefined => {
        Names.camelCase(args.imports, u.name) + "Decoder"
      }
      case m: Generated.Model => {
        Names.camelCase(args.imports, m.name) + "Decoder"
      }
      case u: Container.List => {
        args.imports.addAs("Json.Decode", "Decode")
        "(" + Util.maybeWrapInParens("Decode.list", genDecoderForDatatype(m, f, u.inner)) + ")"
      }
      case u: Container.Option => {
        todo(s"model ${m.name} Field ${f.name} has type option: ${u.name}")
      }
      case u: Container.Map => {
        args.imports.addAs("Json.Decode", "Decode")
        "(" + Util.maybeWrapInParens("Decode.dict", genDecoderForDatatype(m, f, u.inner)) + ")"
      }
    }
  }

  private def todo(msg: String): Nothing = {
    sys.error(s"The elm generator does not yet support this type: $msg")
  }

  private def primitiveDecoder(p: Primitive): String = {
    args.imports.addAs("Json.Decode", "Decode")
    import Datatype.Primitive._
    p match {
      case Boolean => "Decode.bool"
      case Double => "Decode.float"
      case Integer => "Decode.int"
      case Long => "Decode.int"
      case DateIso8601 => {
        args.imports.addAs("Util.DateFormatter", "DateFormatter")
        "DateFormatter.decoder"
      }
      case DateTimeIso8601 => {
        args.imports.addAs("Iso8601", "Iso8601")
        "Iso8601.decoder"
      }
      case Decimal => "Decode.float"
      case Object => "Decode.string" // TODO
      case JsonValue => "Decode.string" // TODO
      case String => "Decode.string"
      case Unit => "(Decode.success Nothing)" // TODO Verify
      case Uuid => "Decode.string"
    }
  }

}
