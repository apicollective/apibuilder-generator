package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

object Client {

  def contents(form: InvocationForm): String =
    scala.models.Play2ClientGenerator(
        scala.models.Play26ClientGenerator.config(form),
        form
      )
      .client

}
