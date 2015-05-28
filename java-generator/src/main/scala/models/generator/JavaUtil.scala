package models.generator

import lib.Text

/**
 *
 * Author: jkenny
 * Date: 28/05/2015
 */
object JavaUtil {
  private val ReservedWords = Set(
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
    "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto",
    "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new",
    "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized",
    "this", "throw", "throws", "transient", "try", "void", "volatile", "while")

  def checkForReservedWord(word: String): String =
    if (ReservedWords.contains(word)) word + "_"
    else word

  def textToComment(text: String): String = textToComment(Seq(text))

  def textToComment(text: Seq[String]): String = {
    "/**\n * " + text.mkString("\n * ") + "\n */"
  }

  def toClassName(modelName: String) = Text.safeName(Text.splitIntoWords(modelName).map(_.toLowerCase.capitalize).mkString)
}
