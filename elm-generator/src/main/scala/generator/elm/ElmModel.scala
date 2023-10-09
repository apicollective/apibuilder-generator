package generator.elm

import io.apibuilder.spec.v0.models.Model

case class ElmModel(imports: Imports) {
  private[this] val elmJson = ElmJson(imports)

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
        s"${Names.camelCase(f.name)}: $opt${ElmType.lookup(f.`type`)}"
      }.mkString("\n, ").indent(4).stripTrailing(),
      "  }"
    ).mkString("\n")
  }


  /*
  memberStatusEncoder : MemberStatus -> Encode.Value
  memberStatusEncoder type_ =
      Encode.string (memberStatusToString type_)
   */
  private[this] def genEncoder(m: Model): String = {
    imports.addAs("Json.Encode", "Encode")
    elmJson.encoder(m.name) {
      Seq(
        "Encode.object",
        "[",
        m.fields.map { f =>

        }.mkString("\n").indent(4),
        "]"
      ).mkString("\n").indent(4)
    }
  }

  /*
  memberStatusDecoder : Decoder MemberStatus
  memberStatusDecoder =
      Decode.map memberStatusFromString string
   */
  private[this] def genDecoder(m: Model): String = {
    elmJson.decoder(m.name) {
      m.fields.map { f =>

      }.mkString("\n").indent(4)
    }
  }
}
