package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

object MockClient {
    def apply(form: InvocationForm): Either[Seq[String], File] =
        scala.generator.mock.MockClientGenerator.Play26
            .invoke(form)
            .flatMap { files =>
                files.headOption
                    .toRight {
                        val error = s"Service[${form.service.organization.key}/${form.service.application.key}] cannot generate play mock client"
                        Seq(error)
                    }
            }
}
