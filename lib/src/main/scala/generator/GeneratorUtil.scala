package lib.generator

import io.apibuilder.spec.v0.models.{Method, Service}
import lib.DatatypeResolver

sealed trait ObjectType
object ObjectType {
  case object Enum extends ObjectType { override def toString = "enums" }
  case object Union extends ObjectType { override def toString = "unions" }
  case object Model extends ObjectType { override def toString = "models" }
  case object Interface extends ObjectType { override def toString = "interfaces" }

  private val all: Seq[ObjectType] = Seq(Enum, Union, Model, Interface)

  def fromString(value: String): Option[ObjectType] = {
    all.find(_.toString == value)
  }
}

object GeneratorUtil {

  /**
    * For convenience to the users of the clients, all generated
    * clients stick all of the models into one namespace -
    * models. This allows, for example, one import statement vs. 3 (1
    * for models, 1 for unions, 1 for enums) when using the generated
    * clients.
    */
  def fullyQualifiedImportName(namespace: String): String = {
    fullyQualifiedExternalName(namespace, ObjectType.Model)
  }

  def fullyQualifiedExternalName(
    namespace: String,
    objectType: ObjectType,
    name: Option[String] = None
  ): String = {
    val base = s"$namespace.$objectType"
    name match {
      case None => base
      case Some(n) => s"$base.$n"
    }
  }

  def datatypeResolver(service: Service): DatatypeResolver = {
    DatatypeResolver(
      enumNames = service.enums.map(_.name) ++ service.imports.flatMap { imp =>
        imp.enums.map(n => fullyQualifiedExternalName(imp.namespace, ObjectType.Enum, Some(n)))
      },
      unionNames = service.unions.map(_.name) ++ service.imports.flatMap { imp =>
        imp.unions.map(n => fullyQualifiedExternalName(imp.namespace, ObjectType.Union, Some(n)))
      },
      modelNames = service.models.map(_.name) ++ service.imports.flatMap { imp =>
        imp.models.map(n => fullyQualifiedExternalName(imp.namespace, ObjectType.Model, Some(n)))
      }
    )
  }

  /**
   * If the provided values share a common prefix, returns the longest
   * string they share. Else None.
   */
  def findLongestCommonPrefix(values: Seq[String]): Option[String] = {
    values match {
      case Nil => None
      case _ => {
        internalFindLongestCommonPrefix("", values) match {
          case "" => None
          case value => Some(value)
        }
      }
    }
  }

  @scala.annotation.tailrec
  private def internalFindLongestCommonPrefix(substring: String, values: Seq[String]): String = {
    values.head.substring(substring.length) match {
      case "" => substring
      case value => {
        val nextSubstring = substring + value(0)
        if (values.forall(_.startsWith(nextSubstring))) {
          internalFindLongestCommonPrefix(nextSubstring, values)
        } else {
          substring
        }
      }
    }
  }

  /**
   * Turns a URL path to a camelcased method name.
   *
   * @param resourcePath The path to the resource itself, if known
   * @param resourceOperationPaths The full set of paths to all operations. This is
   *        used to compute a resource path (longest common string) if resource path
   *        is not explicitly provided.
   */
  def urlToMethodName(
    resourcePath: Option[String],
    resourceOperationPaths: Seq[String],
    method: Method,
    url: String
  ): String = {
    val prefix = resourcePath match {
      case Some(path) => Some(path)
      case None => findLongestCommonPrefix(resourceOperationPaths)
    }

    val operationPath = prefix match {
      case None => url
      case Some(p) => {
        if (url.startsWith(p)) {
          url.substring(p.length)
        } else {
          url
        }
      }
    }

    val pieces = operationPath.split("/").filter(_.nonEmpty)

    val named = pieces.
      filter { _.startsWith(":") }.
      map { name =>
        lib.Text.initCap(lib.Text.safeName(lib.Text.underscoreAndDashToInitCap(name.substring(1))))
      }

    val notNamed = pieces.
      filter { !_.startsWith(":") }.
      map { name =>
        lib.Text.initCap(lib.Text.safeName(lib.Text.underscoreAndDashToInitCap(name)))
      }

    if (named.isEmpty && notNamed.isEmpty) {
      method.toString.toLowerCase

    } else if (named.isEmpty) {
      method.toString.toLowerCase + notNamed.mkString("And")

    } else if (notNamed.isEmpty) {
      method.toString.toLowerCase + "By" + named.mkString("And")

    } else {
      method.toString.toLowerCase + notNamed.mkString("And") + "By" + named.mkString("And")
    }
  }

  /**
   * Splits a string into lines with a given max length
   * leading indentation.
   */
  def splitIntoLines(comment: String, maxLength: Int = 80): Seq[String] = {
    comment.split("\\n").toSeq.flatMap { line =>
      splitLineByLength(line, maxLength)
    }
  }

  private def splitLineByLength(comment: String, maxLength: Int = 80): Seq[String] = {
    val sb = new scala.collection.mutable.ListBuffer[String]()
    var currentWord = new StringBuilder()
    comment.split("\\s+").map(_.trim).foreach { word =>
      if (word.length + currentWord.length >= maxLength) {
        if (currentWord.nonEmpty) {
          sb.append(currentWord.toString)
        }
        currentWord = new StringBuilder()
      } else if (currentWord.nonEmpty) {
        currentWord.append(" ")
      }
      currentWord.append(word)
    }
    if (currentWord.nonEmpty) {
      sb.append(currentWord.toString)
    }
    sb.toList
  }

  /**
   * Format into a multi-line comment w/ a set number of spaces for
   * leading indentation
   */
  def formatComment(comment: String, numberSpaces: Int = 0): String = {
    val spacer = " " * numberSpaces
    splitIntoLines(comment, 80 - 2 - numberSpaces).map { line =>
      s"$spacer# $line"
    }.mkString("\n")
  }

}
