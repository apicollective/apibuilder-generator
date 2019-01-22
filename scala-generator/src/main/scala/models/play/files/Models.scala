package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

object Models {
    def contents(form: InvocationForm): String = {
        val scalaService = scala.generator.ScalaService(form.service)
        scala.generator.ScalaCaseClasses
            .generateCode(scalaService, form.userAgent)
            .headOption
            .fold("")(_.contents)
    }
}
