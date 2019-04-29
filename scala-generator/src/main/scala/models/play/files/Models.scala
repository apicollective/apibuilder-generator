package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Config

object Models {

  def contents(form: InvocationForm): String = {
    val scalaService = scala.generator.ScalaService(form.service, Config(form.attributes, Config.PlayGen2DefaultConfig))
    scala.generator.ScalaCaseClasses
      .generateCode(scalaService, form.userAgent, false)
      .headOption
      .fold("")(_.contents)
  }

}
