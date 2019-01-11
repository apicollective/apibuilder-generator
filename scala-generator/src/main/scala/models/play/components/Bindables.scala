package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator.ScalaService

class Bindables(service: ScalaService) extends Component {

  def code(): ValidatedNel[String, String] = {
    val bindables = scala.models.Play2Bindables(service)
      .build
      .split("\n")
      .drop(1)
      .dropRight(1)
      .mkString("\n")

    val code = s"""
      |package object bindables {
      |${bindables}
      |}
    """

    Validated.validNel(code)
  }

}
