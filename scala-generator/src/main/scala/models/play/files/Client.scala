package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

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

    def file(form: InvocationForm, contents: String): File =
        generator.ServiceFileNames.toFile(
            form.service.namespace,
            form.service.organization.key,
            form.service.application.key,
            form.service.version,
            "Client",
            contents,
            Some("Scala")
        )

    def apply(form: InvocationForm): Either[Seq[String], File] = {
        val contents = this.contents(form)
        val file = this.file(form, contents)

        Right(file)
    }
}
