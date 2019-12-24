package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Attributes

object Models {

  def contents(form: InvocationForm): String = {
    val scalaService = scala.generator.ScalaService(form.service, Attributes.PlayGen2DefaultConfig.withAttributes(form.attributes))
    scala.generator.ScalaCaseClasses
      .generateCode(scalaService, form.userAgent, false)
      .headOption
      .fold("")(_.contents)
  }

}
