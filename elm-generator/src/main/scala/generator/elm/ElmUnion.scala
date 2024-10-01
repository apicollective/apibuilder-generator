package generator.elm

import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.Union

case class ElmUnion(args: GenArgs) {
  private val elmJson = ElmJson(args.imports)

  def generate(union: Union): ValidatedNec[String, String] = {
    genDecoders(union).map { decoders =>
      (
        Seq(genTypeAlias(union)) ++ decoders
      ).mkString("\n\n")
    }
  }

  private def genTypeAlias(union: Union): String = {
    val unionName = Names.pascalCase(union.name)
    Seq(
      s"type $unionName =",
      union.types.map { t =>
        val typeName = Names.pascalCase(t.`type`)
        s"$unionName${typeName} $typeName"
      }.mkString("\n| ").indent(4).stripTrailing(),
    ).mkString("\n")
  }

  private def genDecoders(m: Union): ValidatedNec[String, Seq[String]] = {
    m.discriminator match {
      case None => "Only union types with discriminators are currently supported".invalidNec
      case Some(disc) => {
        Seq(
          genDecoderType(m, disc),
          genDecoderDiscriminator(m, disc),
        ).validNec
      }
    }
  }

  private def decoderByDiscriminatorName(u: Union, disc: String): String = {
    elmJson.decoderName(u.name) + "By" + Names.pascalCase(disc)
  }

  private def genDecoderType(u: Union, disc: String): String = {
    args.imports.addAs("Json.Decode", "Decode")
    elmJson.decoder(u.name) {
      Seq(
        s"Decode.field ${Names.wrapInQuotes(disc)} Decode.string",
        s"    |> Decode.andThen ${decoderByDiscriminatorName(u, disc)}"
      ).mkString("\n")
    }.code
  }

  private def genDecoderDiscriminator(u: Union, disc: String): String = {
    args.imports.addAs("Json.Decode", "Decode")
    val unionName = Names.pascalCase(u.name)
    val name = decoderByDiscriminatorName(u, disc)
    Seq(
      s"$name : String -> Decode.Decoder $unionName",
      s"$name disc =",
      s"    case disc of",
      (u.types.map { t =>
        Seq(
          Names.wrapInQuotes(t.`type`) + " ->",
          s"    ${elmJson.decoderName(t.`type`)} |> Decode.map $unionName${Names.pascalCase(t.`type`)}"
        ).mkString("\n").indent(8).stripTrailing()
      } ++ Seq(
        "_ ->\n    Decode.fail (\"Unknown discriminator: \" ++ disc)".indent(8)
      )
      ).mkString("\n\n").stripTrailing()
    ).mkString("\n").stripTrailing()
  }

}

