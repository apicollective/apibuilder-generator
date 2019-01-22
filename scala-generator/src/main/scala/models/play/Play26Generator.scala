package scala.models.play

import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object Play26Generator extends CodeGenerator {

  def file(form: InvocationForm, suffix: String, contents: String, extension: Option[String]): File =
      generator.ServiceFileNames.toFile(
          form.service.namespace,
          form.service.organization.key,
          form.service.application.key,
          form.service.version,
          suffix,
          contents,
          extension
      )

  def formatScala(contents: String): Either[Throwable, String] = {
      val config = org.scalafmt.config.ScalafmtConfig.default120
      org.scalafmt.Scalafmt.format(contents, config)
        .toEither
  }

  def scalaFiles(form: InvocationForm): Either[Seq[String], List[File]] =
    List(
      ("Client", files.Client.contents(form)),
      ("MockClient", files.MockClient.contents(form)),
      ("Models", files.Models.contents(form)),
      ("ModelsBindables", files.ModelsBindables.contents(form)),
      ("ModelsBodyParsers", files.ModelsBodyParsers.contents(form)),
      ("ModelsJson", files.ModelsJson.contents(form)),
    )
    .traverse { case (suffix, contents) => formatScala(contents).map((suffix, _)) }
    .map(_.map { case (suffix, contents) => file(form, suffix, contents, Some("scala")) })
    .leftMap { t => Seq(t.toString) }

  def routesFile(form: InvocationForm): File =
    File("routes", None, files.Routes.contents(form))

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = for {
    scalaFiles <- this.scalaFiles(form)
    routesFile = this.routesFile(form)
  } yield scalaFiles :+ routesFile

}
