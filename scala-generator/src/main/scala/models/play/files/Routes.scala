package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.Config

object Routes {

  def contents(form: InvocationForm): String = {
    val header = scala.models.ApidocComments(form.service.version, form.userAgent).forPlayRoutes + "\n"
    new scala.models.Play2RouteGenerator(form, Config.PlayGen2DefaultConfig)
      .invoke
      .toOption
      .flatMap(_.headOption)
      .fold("")(_.contents)
      .replaceAll(header, "")
  }

}
