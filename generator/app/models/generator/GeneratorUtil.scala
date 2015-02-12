package generator

import com.gilt.apidoc.spec.v0.models.{Method, ParameterLocation}
import lib.{Datatype, Primitives, Type, Kind}
import lib.Text
import lib.Text._

object GeneratorUtil {

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

case class GeneratorUtil(config: ScalaClientMethodConfig) {

  private def isList(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.List(_) => true
      case _ => false
    }
  }

  private def isMap(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Map(_) => true
      case _ => false
    }
  }

  private def isSingleValue(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Singleton(_) | Datatype.Option(_) => true
      case _ => false
    }
  }

  def queryParameters(
    fieldName: String,
    params: Seq[ScalaParameter]
  ): Option[String] = {
    if (params.isEmpty) {
      None
    } else {
      val listParams = params.filter(p => isList(p.`type`)) match {
        case Nil => Seq.empty
        case params => {
          params.map { p =>
            s"""  ${ScalaUtil.quoteNameIfKeyword(p.name)}.map("${p.originalName}" -> ${p.datatype.primitive.asString("_")})"""
          }
        }
      }
      val arrayParamString = listParams.mkString(" ++\n")

      val singleParams = params.filter(p => isSingleValue(p.`type`)) match {
        case Nil => Seq.empty
        case params => {
          Seq(
            s"val $fieldName = Seq(",
            params.map { p =>
              if (p.isOption) {
                s"""  ${ScalaUtil.quoteNameIfKeyword(p.name)}.map("${p.originalName}" -> ${p.datatype.primitive.asString("_")})"""
              } else {
                s"""  Some("${p.originalName}" -> ${p.datatype.primitive.asString(p.name)})"""
              }
            }.mkString(",\n"),
            ").flatten"
          )
        }
      }
      val singleParamString = singleParams.mkString("\n")

      Some(
        if (singleParams.isEmpty) {
          s"val $fieldName = " + arrayParamString.trim
        } else if (listParams.isEmpty) {
          singleParamString
        } else {
          singleParamString + " ++\n" + arrayParamString
        }
      )
    }
  }

  def pathParams(op: ScalaOperation): String = {
    val pairs = op.pathParameters.map { p =>
      require(p.location == ParameterLocation.Path, "Only singletons can be path parameters.")
      p.originalName -> PathParamHelper.urlEncode(p.name, p.datatype)
    }
    val tmp: String = pairs.foldLeft(op.path) {
      case (path, (name, value)) =>
        val spec = s"/:$name"
        val from = path.indexOfSlice(spec)
        path.patch(from, s"/$${$value}", spec.length)
    }
    s""" s"$tmp" """.trim
  }

  def formBody(op: ScalaOperation): Option[String] = {
    // Can have both or form params but not both as we can only send a single document
    assert(op.body.isEmpty || op.formParameters.isEmpty)

    if (op.formParameters.isEmpty && op.body.isEmpty) {
      None

    } else if (!op.body.isEmpty) {
      val body = op.body.get

      val payload = body.datatype.primitive match {
        case ScalaPrimitive.Enum(ns, name) => ScalaUtil.toVariable(name)
        case ScalaPrimitive.Model(ns, name) => ScalaUtil.toVariable(name)
        case ScalaPrimitive.Union(ns, name) => ScalaUtil.toVariable(name)
        case ScalaPrimitive.String | ScalaPrimitive.DateIso8601 | ScalaPrimitive.DateTimeIso8601 | ScalaPrimitive.Uuid | ScalaPrimitive.Object |
            ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal | ScalaPrimitive.Unit => {
              body.datatype.primitive.asString(ScalaUtil.toVariable(body.`type`))
            }
      }

      Some(s"val payload = play.api.libs.json.Json.toJson($payload)")

    } else {
      val params = op.formParameters.map { param =>
        val varName = ScalaUtil.quoteNameIfKeyword(param.name)
        s""" "${param.originalName}" -> play.api.libs.json.Json.toJson($varName)""".trim
      }.mkString(",\n")
      Some(
        Seq(
          "val payload = play.api.libs.json.Json.obj(",
          params.indent,
          ")"
        ).mkString("\n")
      )
    }
  }

  private object PathParamHelper {
    def urlEncode(
      name: String,
      d: ScalaDatatype
    ): String = {
      d.primitive match {
        case ScalaPrimitive.String => s"""${config.pathEncodingMethod}($name, "UTF-8")"""
        case ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => name
        case ScalaPrimitive.Enum(_, _) => s"""${config.pathEncodingMethod}($name.toString, "UTF-8")"""
        case ScalaPrimitive.DateIso8601 => s"$name.toString"
        case ScalaPrimitive.DateTimeIso8601 => s"${config.pathEncodingMethod}(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($name))"
        case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) | ScalaPrimitive.Object | ScalaPrimitive.Unit => {
          sys.error(s"Cannot encode params of type[$d] as path parameters (name: $name)")
        }
      }
    }
  }

}
