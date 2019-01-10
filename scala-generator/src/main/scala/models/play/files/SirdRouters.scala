package scala.models.play.files

import cats.data.ValidatedNel
import scala.generator.ScalaService
import scala.models.play.components

class SirdRouters(scalaService: ScalaService) extends File {

  def name = "SirdRouters.scala"

  def content(): ValidatedNel[String, String] =
    new components.SirdRouters(scalaService)
      .code
      .map { code =>
        s"""
          |package ${scalaService.namespaces.base}
          |
          |${code}
        """.stripMargin.trim
      }

}
