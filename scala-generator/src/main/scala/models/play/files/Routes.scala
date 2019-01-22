package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

object Routes {
    def apply(form: InvocationForm): Either[Seq[String], File] =
        scala.models.Play2RouteGenerator
            .invoke(form)
            .flatMap { files =>
                files.headOption
                    .toRight {
                        val error = s"Service[${form.service.organization.key}/${form.service.application.key}] cannot generate play routes"
                        Seq(error)
                    }
            }
}
