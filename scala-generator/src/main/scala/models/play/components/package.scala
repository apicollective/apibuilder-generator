package scala.models.play.components

import cats.data._
import scala.generator.ScalaService

trait Component {

    def code(service: ScalaService): ValidatedNel[String, String]

}
