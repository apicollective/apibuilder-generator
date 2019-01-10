package scala.models.play.files

import cats.data.ValidatedNel

trait File {
    def name(): String
    def content(): ValidatedNel[String, String]
}
