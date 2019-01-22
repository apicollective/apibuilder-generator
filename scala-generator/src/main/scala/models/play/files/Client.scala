package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

object Client {
    def apply(form: InvocationForm): Either[Seq[String], File] =
        scala.models.Play26ClientGenerator
            .invoke(form)
            .flatMap { files =>
                files.headOption
                    .toRight {
                        val error = s"Service[${form.service.organization.key}/${form.service.application.key}] cannot generate play client"
                        Seq(error)
                    }
            }
}
