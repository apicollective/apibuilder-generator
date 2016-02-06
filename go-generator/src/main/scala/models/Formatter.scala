package go.models

object Formatter {

  private[this] case class Row(prefix: Option[String], name: String, value: Option[String]) {

    val start = prefix match {
      case None => name
      case Some(p) => s"$p$name"
    }

  }

  private[this] val WhitespacePattern = """^(\s*)([^\s]+)(.*)$""".r


  implicit class Indentable(s: String) {

    private[this] def toOption(s: String): Option[String] = {
      s == "" match {
        case true => None
        case false => Some(s)
      }
    }

    def table(): String = {
      val rows = s.split("\n").map { value =>
        value match {
          case WhitespacePattern(prefix, name, value) => {
            Row(toOption(prefix), name, toOption(value.trim))
          }
          case _ => {
            Row(None, value, None)
          }
        }
      }

      val maxLength = rows.filter(!_.value.isEmpty).map(_.start.length).toList match {
        case Nil => 0
        case els => els.max
      }

      rows.map { row =>
        row.value match {
          case None => row.start
          case Some(v) => {
            val numberSpaces = (maxLength - row.start.length + 1)
            assert(numberSpaces>0, "Must have at least 1 space")
            s"%s%s%s".format(row.start, " " * numberSpaces, v)
          }
        }
      }.mkString("\n")
    }

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

