package models.generator.kotlin

import lib.Text
import com.squareup.kotlinpoet._

trait KotlinUtil {
  // http://kotlinlang.org/docs/reference/keyword-reference.html
  private val ReservedWords = Set(
    "abstract",
    "annotation",
    "as",
    "as?",
    "break",
    "class",
    "companion",
    "continue",
    "crossinline",
    "data",
    "do",
    "enum",
    "external",
    "final",
    "in",
    "infix",
    "inline",
    "inner",
    "internal",
    "lateinit",
    "noinline",
    "open",
    "operator",
    "out",
    "override",
    "private",
    "protected",
    "public",
    "reified",
    "sealed",
    "tailrec",
    "vararg"
  )

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

    if(isModelNameWithPackage(modelName)){
      capitalizeModelNameWithPackage(modelName)
    } else {
      Text.safeName(Text.splitIntoWords(modelName).map { checkForUpperCase(_).capitalize }.mkString)
    }
  }

  def isModelNameWithPackage(modelName: String): Boolean = {
    modelName.toLowerCase.equals(modelName) && modelName.contains(".")
  }

  def capitalizeModelNameWithPackage(modelName: String): String ={
    (Seq(modelName.split("\\.").reverse.head.capitalize) ++ modelName.split("\\.").reverse.tail).reverse.mkString(".")
  }

  def isParameterArray(modelName: String): Boolean = {
    modelName.startsWith("[") && modelName.endsWith("]")
  }

  def getArrayType(modelName: String): String = {
    if (isParameterArray(modelName)) {
      modelName.replaceAll("^\\[", "").replaceAll("\\]$", "")
    } else {
      modelName
    }
  }

  def isParameterMap(modelName: String): Boolean = {
    modelName.startsWith("map[") && modelName.endsWith("]")
  }

  def getMapType(modelName: String): String = {
    if(isParameterMap(modelName)){
      modelName.replaceAll("^.*\\[","").replaceAll("\\]$","")
    } else {
      modelName
    }

  }

  //TODO: we can use primitives as well, but then equal method needs to become smarter, this way is ok

  val dataTypes = Map[String, TypeName](
    "boolean" -> new ClassName("kotlin", "Boolean"),
    "date-iso8601" -> new ClassName("org.joda.time", "DateTime"),
    "date-time-iso8601" -> new ClassName("org.joda.time", "DateTime"),
    "decimal" -> new ClassName("java.math","BigDecimal"),
    "double" -> new ClassName("kotlin","Double"),
    "integer" -> new ClassName("kotlin", "Int"),
    "long" -> new ClassName("kotlin", "Long"),
    "object" -> new ClassName("kotlin.collections","Map"),
    "string" -> new ClassName("kotlin","String"),
    "unit" -> new ClassName("kotlin", "Unit"),
    "uuid" -> new ClassName("java.util","UUID")
  )

  def dataTypeFromField(`type`: String, modelsNameSpace: String): TypeName = {
    dataTypes.get(`type`).getOrElse{
      val name = toParamName(`type`, false)
      if(isParameterArray(`type`)) {
        ParameterizedTypeName.get(new ClassName("kotlin.collections", "List"), dataTypeFromField(getArrayType(`type`), modelsNameSpace))
      }
      else if (isParameterMap(`type`)) {
        ParameterizedTypeName.get(new ClassName("kotlin.collections", "Map"), new ClassName("kotlin", "String"), dataTypeFromField(getMapType(`type`), modelsNameSpace))
      }
      else {
        new ClassName(modelsNameSpace, name)
      }
    }
  }

  def toParamName(modelName: String, startingWithLowercase: Boolean): String = {
    val paramStartingWithUppercase = if (isParameterArray(modelName)){
      toClassName(modelName.tail.reverse.tail.reverse)
    } else {
      toClassName(modelName)
    }
    if(startingWithLowercase){
      checkForReservedWord(paramStartingWithUppercase.head.toLower + paramStartingWithUppercase.tail)
    } else {
      checkForReservedWord(paramStartingWithUppercase)
    }
  }

  def toMethodName(modelName: String): String = {
    val paramStartingWithUppercase = {
      Text.safeName(Text.splitIntoWords(modelName).map { _.toLowerCase.capitalize }.mkString)
    }
    checkForReservedWord(paramStartingWithUppercase)
  }

  def toEnumName(input: String): String = {
    Text.safeName(input.replaceAll("\\.","_")).toUpperCase()
  }

  def makeNameSpace(namespace: String): String = {
    namespace.split("\\.").map { checkForReservedWord }.mkString(".")
  }

}
