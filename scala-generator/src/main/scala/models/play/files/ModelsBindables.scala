package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

object ModelsBindables {

  def contents(form: InvocationForm): String = {
    val scalaService = scala.generator.ScalaService(form.service)
    val bindables = scala.models.Play2Bindables(scalaService)
      .build
      .split("\n")
      .drop(1)
      .dropRight(1)
      .mkString("\n")

    s"""
      package ${scalaService.namespaces.models}

      package object bindables {

        ${bindables}

      }
    """
  }

}
