package scala.models.play

object Helpers {

    implicit class StringOps(s: String) {
        def addMargin(len: Int, marginChar: Char = '|') = {
            val indent = " " * len
            s.stripMargin(marginChar)
                .replace("\n", s"\n${marginChar}${indent}")
        }

        def trimRight(): String = s.replaceAll("\\s+$", "")

        def clean(): String =
            s
            .stripMargin
            .trim
            .lines
            .foldRight(List.empty[String]) {
              case (line, document @ "" :: rest) if line.forall(_.isWhitespace) => document
              case (line, document) => line.trimRight +: document
            }
            .mkString("\n")
    }

}
