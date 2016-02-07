package go.models

object Formatter {

  object Tabulator {
    def format(table: Seq[Seq[String]]): String = {
      table match {
        case Nil => {
          ""
        }
        case _ =>
          val numberColumns: Int = table.map { _.size }.max
          val sizes: Seq[Seq[Int]] = table.map { columns =>
            columns.map { _.length }
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

  private[this] val WhitespacePattern = """^(\s*)([^\s]+)(.*)$""".r

  implicit class Indentable(s: String) {

    def table(): String = {
      val table = s.split("\n").map { value => value.split("\\s+").toSeq }
      val result = Tabulator.format(table.toSeq)

      println("")
      println(s)
      println("")
      println(result)
      println("")
      result
      
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

