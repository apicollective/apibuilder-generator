package scala.models.play

import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
import scala.generator.ScalaService

object Play26Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val scalaService = new ScalaService(form.service)

    val sirdRoutersFile = new files.SirdRouters(scalaService)
    val bindablesFile = new files.Bindables(scalaService)

    (sirdRoutersFile.content(), bindablesFile.content())
      .mapN { case (sirdRoutersFileContent, bindablesFileContent) =>
        Seq(
          File(sirdRoutersFile.name, contents = sirdRoutersFileContent),
          File(bindablesFile.name, contents = bindablesFileContent)
        )
      }
      .toEither
      .leftMap(_.toList)
  }

}
