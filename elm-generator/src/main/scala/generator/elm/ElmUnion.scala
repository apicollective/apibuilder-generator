package generator.elm

import cats.data.ValidatedNec
import cats.implicits.*
import io.apibuilder.spec.v0.models.Union

case class ElmUnion(args: GenArgs) {
  private val elmJson = ElmJson(args.imports)

  def generate(union: Union): ValidatedNec[String, String] = {
    genDecoder(union).map { decoder =>
      (
        Seq(genTypeAlias(union)) ++ Seq(decoder.code)
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

  private def genDecoder(m: Union): ValidatedNec[String, ElmFunction] = {
    m.discriminator match {
      case None => "Only union types with discriminators are currently supported".invalidNec
      case Some(disc) => genDecoderType(m, disc).validNec
    }
  }

  private def decoderByDiscriminatorName(u: Union, disc: String): String = {
    elmJson.decoderName(u.name) + "By" + Names.pascalCase(disc)
  }

  private def genDecoderType(u: Union, disc: String): ElmFunction = {
    args.imports.addAs("Json.Decode", "Decode")
    elmJson.decoder(u.name) {
      Seq(
        s"Decode.field ${Names.wrapInQuotes(disc)} Decode.string",
        s"    |> Decode.andThen (\\disc ->",
        s"        case disc of",
        genDecoderDiscriminator(u, disc).indent(12).stripTrailing,
        s"    )"
      ).mkString("\n")
    }
  }

  private def genDecoderDiscriminator(u: Union, disc: String): String = {
    val unionName = Names.pascalCase(u.name)
    val all = u.types.map { t =>
      s"""
        |${Names.wrapInQuotes(t.`type`)} ->
        |    ${elmJson.decoderName(t.`type`)} |> Decode.map $unionName${Names.pascalCase(t.`type`)}
        |""".stripMargin.strip
    } ++ Seq(s"_ ->\n    Decode.fail (\"Unknown ${Names.maybeQuote(disc)}: \" ++ disc)")
    all.mkString("\n\n").strip()
  }

}

