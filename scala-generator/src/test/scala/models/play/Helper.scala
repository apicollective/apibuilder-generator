package scala.models.play

import org.scalatest.Matchers

object Helpers extends Matchers {
    def removeAllExtraWhiteSpaces(a: String): String = a.replaceAll(" +", " ").trim

    def compareWithoutWhiteSpaces(a: String, b: String) =
        removeAllExtraWhiteSpaces(a) should be(removeAllExtraWhiteSpaces(b))
}
