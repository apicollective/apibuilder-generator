package generator.elm

case class ElmJson(imports: Imports) {

  def encoder(name: String)(contents: String): ElmFunction = {
    imports.addAs("Json.Encode", "Encode")
    val n = s"${Names.camelCase(name)}Encoder"
    val code = Seq(
      s"$n : ${Names.pascalCase(name)} -> Encode.Value",
      s"$n instance =",
      contents.indent(4)
    ).mkString("\n")
    ElmFunction(name = n, code = code)
  }


  def decoder(name: String)(contents: String): ElmFunction = {
    val n = s"${Names.camelCase(name)}Decoder"
    imports.addAs("Json.Decode", "Decode")
    val code = Seq(
      s"$n : Decode.Decoder ${Names.pascalCase(name)}",
      s"$n =",
      contents.indent(4)
    ).mkString("\n")
    ElmFunction(name = n, code = code)
  }
}

case class ElmFunction(name: String, code: String)