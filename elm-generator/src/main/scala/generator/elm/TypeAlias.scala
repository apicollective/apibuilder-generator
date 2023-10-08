package generator.elm

import io.apibuilder.spec.v0.models.Model

object TypeAlias {
  def generate(model: Model): String = {
    Seq(
      s"type alias ${model.name} =",
      model.fields.map { f =>
        val opt = if (f.required) {
          ""
        } else {
          "Maybe "
        }
        s" ${f.name}: ${ElmType.lookup(f.`type`)}$opt"
      }.mkString("\n  ,").trim.stripTrailing()
    ).mkString("\n")
  }
}
