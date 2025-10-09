package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Attributes

object Models {

  def contents(form: InvocationForm): String = {
    val scalaService = scala.generator.ScalaService(form, Attributes.PlayGen2DefaultConfig)
    scala.generator.ScalaCaseClasses
      .generateCode(scalaService, form.userAgent, addHeader = false)
      .headOption
      .fold("")(_.contents)
  }

}
