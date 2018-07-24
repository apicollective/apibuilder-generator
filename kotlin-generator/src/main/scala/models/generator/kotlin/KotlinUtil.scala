package models.generator.kotlin

import com.squareup.kotlinpoet._
import io.apibuilder.spec.v0.models.{Enum, Service}
import lib.Text

trait KotlinUtil {
  val undefinedEnumName = "UNDEFINED"

  // http://kotlinlang.org/docs/reference/keyword-reference.html
  private val ReservedWords = Set(
    "abstract",
    "annotation",
    "as",
    "as?",
    "break",
    "by",
    "catch",
    "class",
    "companion",
    "const",
    "constructor",
    "continue",
    "crossinline",
    "data",
    "delegate",
    "do",
    "dynamic",
    "else",
    "enum",
    "external",
    "false",
    "field",
    "file",
    "final",
    "finally",
    "for",
    "fun",
    "get",
    "if",
    "import",
    "in",
    "!in",
    "infix",
    "init",
    "inline",
    "inner",
    "interface",
    "internal",
    "is",
    "!is",
    "it",
    "lateinit",
    "noinline",
    "null",
    "object",
    "open",
    "operator",
    "out",
    "override",
    "package",
    "param",
    "private",
    "property",
    "protected",
    "public",
    "receiver",
    "reified",
    "return",
    "sealed",
    "set",
    "setparam",
    "super",
    "suspend",
    "tailrec",
    "this",
    "throw",
    "true",
    "try",
    "val",
    "var",
    "vararg",
    "when",
    "where",
    "while"
  )

  def checkForReservedWord(word: String): String =
    if (ReservedWords.contains(word)) word + "_"
    else word

  def textToComment(text: String): String = textToComment(Seq(text))

  def textToComment(text: Seq[String]): String = {
    "/**\n * " + text.mkString("\n * ") + "\n */"
  }

  def toClassName(modelName: String): String = {
    if(isModelNameWithPackage(modelName)){
      capitalizeModelNameWithPackage(modelName)
    } else {
      capitalizeModelName(modelName)
    }
  }

  def isModelNameWithPackage(modelName: String): Boolean = {
    modelName.toLowerCase.equals(modelName) && modelName.contains(".")
  }

  def capitalizeModelName(modelName: String): String ={
    // We don't support upper case class names so if a word is upper case then make it lower case
    def checkForUpperCase(word: String): String =
      if (word == word.toUpperCase) word.toLowerCase
      else word

    Text.safeName(Text.splitIntoWords(modelName).map { checkForUpperCase(_).capitalize }.mkString)
  }

  def capitalizeModelNameWithPackage(modelName: String): String ={
    (Seq(capitalizeModelName(modelName.split("\\.").reverse.head)) ++ modelName.split("\\.").reverse.tail).reverse.mkString(".")
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
    "date-iso8601" -> new ClassName("org.threeten.bp", "LocalDate"),
    "date-time-iso8601" -> new ClassName("org.threeten.bp", "Instant"),
    "decimal" -> new ClassName("java.math","BigDecimal"),
    "double" -> new ClassName("kotlin","Double"),
    "integer" -> new ClassName("kotlin", "Int"),
    "long" -> new ClassName("kotlin", "Long"),
    "object" -> new ClassName("kotlin","Any"),
    "json" -> new ClassName("kotlin","Any"),
    "string" -> new ClassName("kotlin","String"),
    "unit" -> new ClassName("kotlin", "Unit"),
    "uuid" -> new ClassName("java.util","UUID")
  )



  def dataTypeFromField(`type`: String, serviceNameSpace: String, service: Service): TypeName = {
    dataTypeFromField(`type`, serviceNameSpace, service.enums)
  }

  protected def dataTypeFromField(`type`: String, serviceNameSpace: String, allEnums: Seq[Enum]): TypeName = {
    dataTypes.get(`type`).getOrElse{
      val name = toParamName(`type`, false)
      if(isParameterArray(`type`)) {
        ParameterizedTypeName.get(new ClassName("kotlin.collections", "List"), dataTypeFromField(getArrayType(`type`), serviceNameSpace, allEnums))
      }
      else if (isParameterMap(`type`)) {
        ParameterizedTypeName.get(new ClassName("kotlin.collections", "Map"), new ClassName("kotlin", "String"), dataTypeFromField(getMapType(`type`), serviceNameSpace, allEnums))
      }
      else {
        val isLocalEnum = allEnums.exists(enum => enum.name == `type`)
        val nameSpace: String = if(isLocalEnum) toEnumsNameSpace(serviceNameSpace) else toModelsNameSpace(serviceNameSpace)
        new ClassName(nameSpace, name)
      }
    }
  }

  def toModelsNameSpace(nameSpace: String): String = nameSpace + ".models"

  def toEnumsNameSpace(nameSpace: String): String = nameSpace + ".enums"

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
    val paramStartingWithLowercase = {
      Text.initLowerCase(Text.safeName(Text.splitIntoWords(modelName).map { _.toLowerCase.capitalize }.mkString))
    }
    checkForReservedWord(paramStartingWithLowercase)
  }

  def toEnumName(input: String): String = {
    if (input == undefinedEnumName) {
      input
    } else {
      Text.safeName(input.replaceAll("\\.", "_").replaceAll("-","_")).toUpperCase
    }
  }

  def makeNameSpace(namespace: String): String = {
    namespace.split("\\.").map { checkForReservedWord }.mkString(".")
  }

  def getThrowableClassName() : ClassName = {
    new ClassName("kotlin", "Throwable")
  }

  def getKotlinIntClassName() : ClassName = {
    new ClassName("kotlin", "Int")
  }

  def getKotlinStringClassName() : ClassName = {
    new ClassName("kotlin", "String")
  }

}
