package scala.models.play

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object Play26Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val values = List(
      files.Client(form),
      files.MockClient(form),
      files.Models(form),
      files.ModelsJson(form),
      files.ModelsBindables(form),
      files.Routes(form),
    )
    .flatMap {
      case Right(file) => List(file)
      case Left(errors) =>
        println(errors)
        List.empty
    }

    Right(values)
  }
}
