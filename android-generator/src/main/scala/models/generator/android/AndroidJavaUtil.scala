package models.generator.android

import lib.Text
import com.squareup.javapoet.FieldSpec

/**
 *
 * Author: jkenny
 * Date: 28/05/2015
 */
trait AndroidJavaUtil {
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

  def toClassName(modelName: String): String = {
    // We don't support upper case class names so if a word is upper case then make it lower case
    def checkForUpperCase(word: String): String =
      if (word == word.toUpperCase) word.toLowerCase
      else word

    Text.safeName(Text.splitIntoWords(modelName).map { checkForUpperCase(_).capitalize }.mkString)
  }

  def isParameterArray(modelName: String): Boolean = {
    modelName.startsWith("[") && modelName.endsWith("]")
  }

  def toParamName(modelName: String, startingWithLowercase: Boolean): String = {
    val paramStartingWithUppercase = if (isParameterArray(modelName)){
      toClassName(modelName.tail.reverse.tail.reverse)
    } else {
      toClassName(modelName)
    }
    if(startingWithLowercase){
      paramStartingWithUppercase.head.toLower + paramStartingWithUppercase.tail
    } else {
      paramStartingWithUppercase
    }

  }


  def toEnumName(input: String): String = {
    toClassName(input).replaceAll(".","")
  }

  def makeNameSpace(namespace: String): String = {
    namespace.split("\\.").map { checkForReservedWord }.mkString(".")
  }

}
