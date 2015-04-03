package generator

import com.gilt.apidoc.spec.v0.models.{Method, ParameterLocation, Service}
import lib.{Datatype, DatatypeResolver, Primitives, Type, Kind}
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
      case Datatype.Singleton(_) => true
      case Datatype.List(_) | Datatype.Map(_) => false
    }
  }

  def queryParameters(
    fieldName: String,
    params: Seq[ScalaParameter]
  ): Option[String] = {
    if (params.isEmpty) {
      None
    } else {
      val listParams = params.map(p => p -> p.datatype).collect {
        case (p, ScalaDatatype.Option(ScalaDatatype.List(inner))) => {
          s"""  ${ScalaUtil.quoteNameIfKeyword(p.name)}.getOrElse(Nil).map("${p.originalName}" -> ${inner.asString("_")})"""
        }
        case (p, ScalaDatatype.List(inner)) => {
          s"""  ${ScalaUtil.quoteNameIfKeyword(p.name)}.map("${p.originalName}" -> ${inner.asString("_")})"""
        }
      }
      val arrayParamString = listParams.mkString(" ++\n")

      val singleParams = params.filter(p => isSingleValue(p.`type`)) match {
        case Nil => Seq.empty
        case params => Seq(
          s"val $fieldName = Seq(",
          params.map(p => p -> p.datatype).collect {
            case (p, ScalaDatatype.Option(inner)) => {
              s"""  ${ScalaUtil.quoteNameIfKeyword(p.name)}.map("${p.originalName}" -> ${inner.asString("_")})"""
            }
            case (p, dt: ScalaPrimitive) => s"""  Some("${p.originalName}" -> ${dt.asString(p.name)})"""
          }.mkString(",\n"),
          ").flatten"
        )
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

      val payload = body.datatype match {
        case p: ScalaPrimitive => ScalaUtil.toDefaultVariable(multiple = false)
        case ScalaDatatype.List(t) => ScalaUtil.toVariable(t.shortName, multiple = true)
        case ScalaDatatype.Map(t) => t.asString(ScalaUtil.toVariable(body.`type`))
        case c: ScalaDatatype.Option => sys.error(s"unsupported container type ${c} encountered as body for $op")
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
      d match {
        case ScalaPrimitive.String => s"""${config.pathEncodingMethod}($name, "UTF-8")"""
        case ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => name
        case ScalaPrimitive.Enum(_, _) => s"""${config.pathEncodingMethod}($name.toString, "UTF-8")"""
        case ScalaPrimitive.DateIso8601 => s"$name.toString"
        case ScalaPrimitive.DateTimeIso8601 => s"${config.pathEncodingMethod}(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($name))"
        case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) | ScalaPrimitive.Object | ScalaPrimitive.Unit => {
          sys.error(s"Cannot encode params of type[$d] as path parameters (name: $name)")
        }
        case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encounteered as path param($name)")
      }
    }
  }

}
