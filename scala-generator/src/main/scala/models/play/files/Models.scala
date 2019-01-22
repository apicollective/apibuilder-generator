package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

object Models {
    def apply(form: InvocationForm): Either[Seq[String], File] =
        scala.generator.ScalaCaseClasses
            .invoke(form, addHeader = true)
            .flatMap { files =>
                files.headOption
                    .map { file =>
                        generator.ServiceFileNames.toFile(
                            form.service.namespace,
                            form.service.organization.key,
                            form.service.application.key,
                            form.service.version,
                            "Models",
                            file.contents,
                            Some("Scala")
                        )
                    }
                    .toRight {
                        val error = s"Service[${form.service.organization.key}/${form.service.application.key}] cannot generate play models"
                        Seq(error)
                    }
            }
}
