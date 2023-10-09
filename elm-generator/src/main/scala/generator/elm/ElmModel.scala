package generator.elm

import io.apibuilder.spec.v0.models.Model

case class ElmModel(imports: Imports) {
  def generate(model: Model): String = {
    Seq(
      genTypeAlias(model),
      //genEncoder(model),
      //genDecoder(model)
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

}
