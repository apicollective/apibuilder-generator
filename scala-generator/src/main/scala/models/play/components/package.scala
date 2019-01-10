package scala.models.play.components

import cats.data._

trait Component {

    def code(): ValidatedNel[String, String]

}
