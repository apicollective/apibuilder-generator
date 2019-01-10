package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator.ScalaService

class Bindables(service: ScalaService) extends Component {

  def code(): ValidatedNel[String, String] = {
    val code = scala.models.Play2Bindables(service).build
    Validated.validNel(code)
  }

}
