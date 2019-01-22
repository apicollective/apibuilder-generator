package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

object Client {
    def contents(form: InvocationForm): String = {
        val header = scala.models.ApidocComments(form.service.version, form.userAgent)

        val gen = scala.models.Play2ClientGenerator(
            scala.models.Play26ClientGenerator.config(form),
            form
        )

        s"""
            ${header.toJavaString()}

            ${gen.client()}
        """
    }
}
