package generator

import com.gilt.apidoc.spec.v0.models.{Method, ParameterLocation, Service}
import lib.{Datatype, DatatypeResolver}
import lib.Text
import lib.Text._

object GeneratorUtil {

  def datatypeResolver(service: Service): DatatypeResolver = {
    DatatypeResolver(
      enumNames = service.enums.map(_.name) ++ service.imports.flatMap { imp =>
        imp.enums.map(name => s"${imp.namespace}.enums.${name}")
      },
      unionNames = service.unions.map(_.name) ++ service.imports.flatMap { imp =>
        imp.unions.map(name => s"${imp.namespace}.unions.${name}")
      },
      modelNames = service.models.map(_.name) ++ service.imports.flatMap { imp =>
        imp.models.map(name => s"${imp.namespace}.models.${name}")
      }
    )
  }

  /**
   * Turns a URL path to a camelcased method name.
   */
  def urlToMethodName(
    resourcePlural: String,
    method: Method,
    url: String
  ): String = {
    val pathsToSkip = Seq(
      resourcePlural,
      formatName(resourcePlural),
      formatName(resourcePlural).toLowerCase
    )

    val resourcePath = formatName(resourcePlural)

    val pieces = url.split("/").filter { !_.isEmpty }.filter { w => !pathsToSkip.contains(formatName(w)) }

    val named = pieces.filter { _.startsWith(":") }.map { name =>lib.Text.initCap(lib.Text.safeName(lib.Text.underscoreAndDashToInitCap(name.slice(1, name.length)))) }
    val notNamed = pieces.
      filter { !_.startsWith(":") }.
      filter { _ != resourcePlural.toLowerCase }.
      map( name =>lib.Text.initCap(lib.Text.safeName(lib.Text.underscoreAndDashToInitCap(name))) )

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
    * Creates a canonical form for the specified name. Eg
    * MembershipRequest and membership-request both end up as
    * membership_request
    */
  private def formatName(name: String): String = {
    Text.splitIntoWords(Text.camelCaseToUnderscore(name)).mkString("_")
  }

  /**
   * Splits a string into lines with a given max length
   * leading indentation.
   */
  def splitIntoLines(comment: String, maxLength: Int = 80): Seq[String] = {
    val sb = new scala.collection.mutable.ListBuffer[String]()
    var currentWord = new StringBuilder()
    comment.split(" ").map(_.trim).foreach { word =>
      if (word.length + currentWord.length >= maxLength) {
        if (!currentWord.isEmpty) {
          sb.append(currentWord.toString)
        }
        currentWord = new StringBuilder()
      } else if (!currentWord.isEmpty) {
        currentWord.append(" ")
      }
      currentWord.append(word)
    }
    if (!currentWord.isEmpty) {
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
