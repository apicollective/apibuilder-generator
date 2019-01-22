package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

object Routes {

    def contents(form: InvocationForm): String =
        scala.models.Play2RouteGenerator
            .invoke(form)
            .toOption
            .flatMap(_.headOption)
            .fold("")(_.contents)

}
