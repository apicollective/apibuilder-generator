package go.models

object Formatter {

  implicit class Indentable(s: String) {

    def indent(numberTabs: Int): String = {
      s.split("\n").map { value =>
        if (value.trim == "") {
          ""
        } else {
          ("\t" * numberTabs) + value
        }
      }.mkString("\n")
    }
  }

}

