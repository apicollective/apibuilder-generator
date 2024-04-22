package generator.elm

object Util {

  def maybeWrapInParens(prefix: String, contents: String): String = {
    s"$prefix ${maybeWrapInParens(contents)}"
  }

  def maybeWrapInParens(contents: String): String = {
    val i = contents.indexOf(" ")
    if (i > 0) {
      s"($contents)"
    } else {
      s"$contents"
    }
  }
}
