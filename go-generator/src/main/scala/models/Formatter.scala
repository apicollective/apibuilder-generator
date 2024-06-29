package go.models

object Formatter {

  private val LeadingWhitespace = """^(\s+)(.*)$""".r

  implicit class Indentable(s: String) {

    def table(): String = {
      val table = s.split("\n").map { value =>
        isComment(value) match {
          case true => {
            Seq(value)
          }
          case false => {
            value match {
              case LeadingWhitespace(spaces, text) => {
                // Merge leading whitespace into a single first element -
                // e.g. "  a" becomes a list with one element (instead of
                // a two element list with the first element just being
                // whitespace)
                val values = splitLine(text)
                Seq(s"$spaces${values(0)}") ++ values.drop(1)
              }
              case _ => {
                splitLine(value)
              }
            }
          }
        }
      }

      formatTable(table.toSeq)
    }

    private def splitLine(line: String): Seq[String] = {
      line.split("\\=").toList match {
        case Nil => Nil
        case one :: Nil => one.split("\\s+").toSeq
        case one :: rest => Seq(one) ++ Seq("=") ++ splitLine(rest.mkString("=").trim)
      }
    }

    def indentString(numberTabs: Int): String = {
      s.split("\n").map { value =>
        if (value.trim == "") {
          ""
        } else {
          ("\t" * numberTabs) + value
        }
      }.mkString("\n")
    }

    private def isComment(value: String): Boolean = {
      value.trim.startsWith("//")
    }

    private def formatTable(table: Seq[Seq[String]]): String = {
      table match {
        case Nil => {
          ""
        }
        case _ =>
          val numberColumns: Int = table.map { _.size }.max
          val sizes: Seq[Seq[Int]] = table.map { columns =>
            columns.map { value =>
              isComment(value) match {
                case true => 0
                case false => value.length
              }
            }
          }
          val maxSizes: Seq[Int] = 0.to(numberColumns).map { case i =>
            sizes.map { _.lift(i).getOrElse(0) }.max
          }

          table.map { values =>
            values.zipWithIndex.map { case (v, i) =>
              // Don't leave trailing spaces
              if (i >= values.length - 1) {
                v
              } else {
                v + (" " * (maxSizes(i) - v.length))
              }
            }.mkString(" ")
          }.mkString("\n")
      }
    }
    
  }

}

