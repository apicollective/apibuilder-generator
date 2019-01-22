package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

object MockClient {

  def contents(form: InvocationForm): String = {
    val scalaService = scala.generator.ScalaService(form.service)
    val config = scala.generator.ScalaClientMethodConfigs.Play26(scalaService.namespaces.base, None)

    new scala.generator.mock.MockClientGenerator(scalaService, form.userAgent, config)
      .generateCode
  }

}
