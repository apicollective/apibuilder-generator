package generator.elm

case class ElmJson(imports: Imports) {

  def encoder(name: String)(contents: String): String = {
    imports.addAs("Json.Encode", "Encode")
    Seq(
      s"${Names.camelCase(name)}Encoder : ${Names.pascalCase(name)} -> Encode.Value",
      s"${Names.camelCase(name)}Encoder instance =",
      contents.indent(4)
    ).mkString("\n")
  }

  def decoder(name: String)(contents: String): String = {
    imports.addAs("Json.Decode", "Decode")
    Seq(
      s"${Names.camelCase(name)}Decoder : Decode.Decoder ${Names.pascalCase(name)}",
      s"${Names.camelCase(name)}Decoder =",
      contents.indent(4)
    ).mkString("\n")
  }
}
