package scala.models.play

import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
import scala.generator.ScalaService

object Play26Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val scalaService = new ScalaService(form.service)

    List(
      new files.Bindables(scalaService),
      new files.BodyParsers(scalaService),
      new files.Controllers(scalaService),
      new files.Json(scalaService),
      new files.SirdRouters(scalaService)
    )
      .traverse(_.file)
      .toEither
      .leftMap(_.toList)
  }

}
