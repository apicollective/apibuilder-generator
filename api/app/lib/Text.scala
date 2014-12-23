package lib

object Text {

  implicit class Indentable(s: String) {
    def indent: String = indent(2)
    def indent(width: Int): String = {
      s.split("\n").map { value =>
        if (value.trim == "") {
          ""
        } else {
          (" " * width) + value
        }
      }.mkString("\n")
    }
  }

}
