package scala.models.play

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object Play26Generator extends CodeGenerator {

  def format(file: File): Either[Throwable, File] = file match {
    case scalaFile if scalaFile.name.endsWith(".scala") =>
      val config = org.scalafmt.config.ScalafmtConfig.default120
      org.scalafmt.Scalafmt.format(scalaFile.contents, config)
        .toEither
        .map { formatted => scalaFile.copy(contents = formatted) }

    case file => Right(file)
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val values = List(
      files.Client(form),
      files.MockClient(form),
      files.Models(form),
      files.ModelsBindables(form),
      files.ModelsBodyParsers(form),
      files.ModelsJson(form),
      files.Routes(form),
    )
    .map(_.flatMap(format))
    .flatMap {
      case Right(file) => List(file)
      case Left(errors) =>
        println(errors)
        List.empty
    }

    Right(values)
  }
}
