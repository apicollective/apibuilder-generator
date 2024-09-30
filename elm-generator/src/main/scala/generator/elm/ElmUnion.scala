package generator.elm

import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.Union

case class ElmUnion(args: GenArgs) {
  private val elmJson = ElmJson(args.imports)
  private val elmType = ElmTypeLookup(args)

  def generate(union: Union): ValidatedNec[String, String] = {
    genDecoders(union).map { decoders =>
      (
        Seq(genTypeAlias(union)) ++ decoders.map(_.code)
      ).mkString("\n\n")
    }
  }

  private def genTypeAlias(union: Union): String = {
    val unionName = Names.pascalCase(union.name)
    Seq(
      s"type $unionName =",
      "    |",
      union.types.map { t =>
        s"$unionName${Names.pascalCase(t.`type`)}"
      }.mkString("\n|").indent(4).stripTrailing(),
    ).mkString("\n")
  }

  private def genDecoders(m: Union): ValidatedNec[String, Seq[ElmFunction]] = {
    m.discriminator match {
      case None => "Only union types with discriminators are currently supported".invalidNec
      case Some(disc) => {
        val discDecoder = genDecoderDiscriminator(m, disc)
        Seq(
          genDecoderType(m, disc, discDecoder),
          discDecoder,
        ).validNec
      }
    }
  }

  private def genDecoderType(u: Union, disc: String, discriminatorDecoder: ElmFunction): ElmFunction = {
    args.imports.addAs("Json.Decode", "Decode")
     elmJson.decoder(u.name) {
        Seq(
          s"Decode.field ${Names.wrapInQuotes(disc)} Decode.string",
          s"    |> Decode.andThen ${discriminatorDecoder.name}"
        ).mkString("\n").indent(4)
     }
  }

  private def genDecoderDiscriminator(u: Union, disc: String): ElmFunction = {
    args.imports.addAs("Json.Decode", "Decode")
    elmJson.decoder(u.name + "_by_discriminator") {
      Seq(
        s"case ${Names.maybeQuote(disc)} of",
        u.types.map { t =>
          Seq(
            Names.wrapInQuotes(t.`type`) + " ->",
            s"    ${elmJson.decoderName(t.`type`)} |> Decode.map ${Names.pascalCase(u.name)}"

          ).mkString("\n")
        }
      ).mkString("\n").indent(4)
    }
  }

}

