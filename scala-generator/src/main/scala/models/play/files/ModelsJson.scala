package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

object ModelsJson {
    def apply(form: InvocationForm): Either[Seq[String], File] =
        scala.models.Play2Models
            .generateCode(form, addBindables = false, addHeader = true)
            .headOption
            .map { file =>
                generator.ServiceFileNames.toFile(
                    form.service.namespace,
                    form.service.organization.key,
                    form.service.application.key,
                    form.service.version,
                    "ModelsJson",
                    file.contents,
                    Some("Scala")
                )
            }
            .toRight {
                val error = s"Service[${form.service.organization.key}/${form.service.application.key}] cannot generate play models, and json"
                Seq(error)
            }
}
