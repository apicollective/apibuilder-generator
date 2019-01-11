package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator.ScalaService
import scala.models.play.Helpers._

class Models(service: ScalaService) extends Component {

  def code(): ValidatedNel[String, String] = {
    val caseClasses = scala.generator.ScalaCaseClasses
      .generateCodeBody(service)

    val code = s"""
      |package object models {
      |  ${caseClasses.addMargin(2)}
      |}
    """

    Validated.validNel(code)
  }

}
