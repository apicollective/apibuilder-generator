package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Config

object Client {

  def contents(form: InvocationForm): String =
    scala.models.Play2ClientGenerator(
        scala.models.Play26ClientGenerator.config(form),
        form,
        Config.PlayGen2DefaultConfig
      )
      .client

}
