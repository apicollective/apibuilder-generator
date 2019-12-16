package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Attributes

object Routes {

  def contents(form: InvocationForm): String = {
    val header = scala.models.ApidocComments(form.service.version, form.userAgent).forPlayRoutes + "\n"
    scala.models.Play2RouteGenerator(form, Attributes.PlayGen2DefaultConfig)
      .invoke
      .toOption
      .flatMap(_.headOption)
      .fold("")(_.contents)
      .replaceAll(header, "")
  }

}
