package scala.models.play.files

import cats.data.ValidatedNel
import scala.generator.ScalaService
import scala.models.play.components
import scala.models.play.Helpers._

class Controllers(scalaService: ScalaService) extends File {

  def name() = "Controllers.scala"
  def content(): ValidatedNel[String, String] =
    new components.Controllers(scalaService)
      .code
      .map { code =>
        s"""
          |package ${scalaService.namespaces.base}
          |
          |${code}
        """.clean
      }

}
