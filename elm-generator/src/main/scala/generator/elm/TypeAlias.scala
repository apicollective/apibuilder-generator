package generator.elm

import io.apibuilder.spec.v0.models.Model
import lib.Text._

object TypeAlias {
  def generate(model: Model): String = {
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
