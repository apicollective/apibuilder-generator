package scala.models.play.components

import cats.data._

trait Component {

    def code(): ValidatedNel[String, String]

    implicit class StringOps(s: String) {
        def addMargin(len: Int, marginChar: Char = '|') = {
            val indent = " " * len
            s.stripMargin(marginChar)
                .replace("\n", s"\n${marginChar}${indent}")
        }
    }

}
