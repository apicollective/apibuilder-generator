package scala.models.play

import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
import scala.generator.ScalaService

object Play26Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val scalaService = new ScalaService(form.service)

    val bindablesFile = new files.Bindables(scalaService)
    val sirdRoutersFile = new files.SirdRouters(scalaService)
    val controllersFile = new files.Controllers(scalaService)

    (bindablesFile.content(), sirdRoutersFile.content(), controllersFile.content())
      .mapN { case (sirdRoutersFileContent, bindablesFileContent, controllersFileContent) =>
        Seq(
          File(bindablesFile.name, contents = bindablesFileContent),
          File(sirdRoutersFile.name, contents = sirdRoutersFileContent),
          File(controllersFile.name, contents = controllersFileContent),
        )
      }
      .toEither
      .leftMap(_.toList)
  }

}
