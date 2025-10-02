package models.generator.android

import lib.Text
import com.squareup.javapoet.*

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

    if(isModelNameWithPackage(modelName)){
      replaceEnumsPrefixWithModels(capitalizeModelNameWithPackage(modelName))
    } else {
      Text.safeName(Text.splitIntoWords(modelName).map { checkForUpperCase(_).capitalize }.mkString)
    }
  }

  /**
    * Because we put enums models directory and package (scala client does the same), we need to replace
    * a.b.c.d.enums.EnumName with a.b.c.d.models.EnumName
    * @param str
    * @return
    */

  def replaceEnumsPrefixWithModels(str: String): String ={
    val arr = str.split("\\.")
    val updatedArr = (arr.reverse.head +: (if(arr.reverse.tail.head == "enums") "models" else arr.reverse.tail.head) +: arr.reverse.tail.tail).reverse
    updatedArr.mkString(".")
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
      modelName.substring(modelName.indexOf("[")+1).replaceAll("\\]$","")
    } else {
      modelName
    }

  }

  //TODO: we can use primitives as well, but then equal method needs to become smarter, this way is ok

  val dataTypes = Map[String, TypeName](
    "boolean" -> ClassName.get("java.lang", "Boolean"),
    "date-iso8601" -> ClassName.get("org.joda.time", "DateTime"),
    "date-time-iso8601" -> ClassName.get("org.joda.time", "DateTime"),
    "decimal" -> ClassName.get("java.math","BigDecimal"),
    "double" -> ClassName.get("java.lang","Double"),
    "integer" -> ClassName.get("java.lang", "Integer"),
    "long" -> ClassName.get("java.lang", "Long"),
    "object" -> ClassName.get("java.util","Map"),
    "json" -> ClassName.get("java.lang","Object"),
    "string" -> ClassName.get("java.lang","String"),
    "unit" -> ClassName.get("java.lang", "Void"),
    "uuid" -> ClassName.get("java.util","UUID")
  )

  def dataTypeFromField(`type`: String, modelsNameSpace: String): TypeName = {
    dataTypes.get(`type`).getOrElse{
      val name = toParamName(`type`, false)
      if(isParameterArray(`type`))
        ArrayTypeName.of(dataTypeFromField(getArrayType(`type`), modelsNameSpace))
      else if (isParameterMap(`type`))
        ParameterizedTypeName.get(ClassName.get("java.util", "Map"), ClassName.get("java.lang", "String"), dataTypeFromField(getMapType(`type`), modelsNameSpace))
      else
        ClassName.get(modelsNameSpace, name)
    }
  }

  def toParamName(modelName: String, startingWithLowercase: Boolean): String = {
    val paramStartingWithUppercase = if (isParameterArray(modelName)){
      toClassName(modelName.tail.reverse.tail.reverse)
    } else {
      toClassName(modelName)
    }
    if(startingWithLowercase){
      checkForReservedWord(s"${paramStartingWithUppercase.head.toLower}${paramStartingWithUppercase.tail}")
    } else {
      checkForReservedWord(paramStartingWithUppercase)
    }
  }

  def toMethodName(modelName: String): String = {
    val methodName = {
      val methodNameStartingWithUpperCase = Text.splitIntoWords(modelName).map {
        _.toLowerCase.capitalize
      }.mkString
      Text.safeName(s"${methodNameStartingWithUpperCase.head.toLower}${methodNameStartingWithUpperCase.tail}")
    }
    checkForReservedWord(methodName)
  }

  def toEnumName(input: String): String = {
    Text.safeName(input.replaceAll("\\.","_").replaceAll("-","_")).toUpperCase()
  }

  def makeNameSpace(namespace: String): String = {
    namespace.split("\\.").map { checkForReservedWord }.mkString(".")
  }

}
