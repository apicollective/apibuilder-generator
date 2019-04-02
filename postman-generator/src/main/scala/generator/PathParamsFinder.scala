package generator

object PathParamsFinder {

  private val regex = """\:(\w+)[\/]{0,1}""".r

  def find(path: String): Seq[String] = {
    regex
      .findAllIn(path)
      .map(str => regex.replaceAllIn(str, "$1"))
      .toList
  }
}
