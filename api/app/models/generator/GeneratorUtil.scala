package generator

import com.gilt.apidocgenerator.models.{Method}
import lib.{Primitives, Type, TypeKind}
import lib.Text._

object GeneratorUtil {

  /**
   * Turns a URL path to a camelcased method name.
   */
  def urlToMethodName(
    resourcePlural: String,
    resourcePath: String,
    method: Method,
    url: String
  ): String = {
    val pieces = (if (resourcePath.startsWith("/:")) {
      url
    } else {
      url.replaceAll("^" + resourcePath, "")
    }).split("/").filter { !_.isEmpty }

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
  def params(
    fieldName: String,
    params: Seq[ScalaParameter]
  ): Option[String] = {
    if (params.isEmpty) {
      None
    } else {
      val listParams = params.map { p =>
        p.datatype match {
          case ScalaDatatype.List(single :: Nil) => {
            Some(s"""  ${p.name}.map("${p.originalName}" -> ${single.asString("_")})""")
          }
          case ScalaDatatype.List(multiple) => {
            sys.error("TODO: UNION TYPE")
          }
          case other => {
            None
          }
        }
      }.flatten

      val arrayParamString = listParams.mkString(" ++\n")

      val mapParams = params.map { p =>
        p.datatype match {
          case ScalaDatatype.Map(inner) => {
            sys.error("TODO: Finish map")
          }
          case other => {
            None
          }
        }
      }

      // TODO: Finish map val mapParamString = listParams.mkString(" ++\n")

      val singleParams: Option[String] = params.map { p =>
        p.datatype.types match {
          case (single :: Nil) => {
            if (p.isOption) {
              s"""  ${p.name}.map("${p.originalName}" -> ${single.asString("_")})"""
            } else {
              s"""  Some("${p.originalName}" -> ${single.asString(p.name)})"""
            }
          }
          case (multiple) => {
            sys.error("TODO: UNION TYPE")
          }
        }
      }.flatten match {
        case Nil => None
        case params => {
          Some(
            Seq(
              s"val $fieldName = Seq(",
              params.mkString(",\n"),
              ").flatten"
            ).mkString("\n")
          )
        }
      }

      Some(
        singleParams match {
          case None => {
            s"val $fieldName = " + arrayParamString.trim
          }
          case Some(value) => {
            if (listParams.isEmpty) {
              value
            } else {
              value + " ++\n" + arrayParamString
            }
          }
        }
      )
    }
  }

  def pathParams(op: ScalaOperation): String = {
    val pairs = op.pathParameters.map { p =>
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
      val name = op.body.get.name

      val payload = body.datatype.types match {
        case (single :: Nil) => {
          single.asString(ScalaUtil.toDefaultVariable(multiple = body.multiple))
        }
        case (multiple) => {
          sys.error("TODO: UNION TYPE")
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
      d.types match {
        case (single :: Nil) => {
          single match {
            case ScalaPrimitive.String => s"""${config.pathEncodingMethod}($name, "UTF-8")"""
            case ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => name
            case ScalaPrimitive.Enum(_) => s"""${config.pathEncodingMethod}($name.toString, "UTF-8")"""
            case ScalaPrimitive.DateIso8601 => s"$name.toString"
            case ScalaPrimitive.DateTimeIso8601 => s"${config.pathEncodingMethod}(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($name))"
            case ScalaPrimitive.Model(_) | ScalaPrimitive.Object | ScalaPrimitive.Unit => {
              sys.error(s"Cannot encode params of type[$d] as path parameters (name: $name)")
            }
          }
        }
        case (multiple) => {
          sys.error("TODO: UNION TYPE")
        }
      }
    }
  }

}
