package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Attributes

object Client {

  def contents(form: InvocationForm): String =
    scala.models.Play2ClientGenerator(
        scala.models.Play26ClientGenerator.config(form),
        form,
        Attributes.PlayGen2DefaultConfig
      )
      .client

}
