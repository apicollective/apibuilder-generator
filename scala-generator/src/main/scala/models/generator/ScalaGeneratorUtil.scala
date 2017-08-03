package scala.generator

import io.apibuilder.spec.v0.models.ParameterLocation
import lib.Datatype
import lib.Text._
import lib.generator.GeneratorUtil

object ScalaGeneratorUtil {

  /**
    * Generates the scala doc given an optional description and a list of parameters
    * @param params a list of parameters as (key, optional description)
    */
  def scaladoc(description: Option[String], params: Seq[(String, Option[String])]): String = {
    internalScaladoc(description, params) match {
      case None => ""
      case Some(d) => d + "\n"
    }
  }

  private[this] def internalScaladoc(description: Option[String], params: Seq[(String, Option[String])]): Option[String] = {
    val modelDesc = description.map(_.trim).filter(_.nonEmpty)

    val prefix = s"@param "
    val paramDesc: Seq[String] = params.flatMap { case (name, optionalDescription) =>
      optionalDescription.map(_.trim).filter(_.nonEmpty).map { desc =>
        val lines = GeneratorUtil.splitIntoLines(desc).map { _.indent(prefix.length) }
        s"$prefix$name " + lines.mkString("\n").trim
      }
    }

    (modelDesc, paramDesc) match {
      case (None, Nil) => None
      case (Some(m), Nil) => Some(ScalaUtil.textToComment(m))
      case (None, p) => Some(ScalaUtil.textToComment(p.mkString("\n").split("\n")))
      case (Some(m), p) => Some(
        ScalaUtil.textToComment(
          (GeneratorUtil.splitIntoLines(m).mkString("\n") + "\n\n" + p.mkString("\n")).split("\n")
        )
      )
    }
  }

}

class ScalaGeneratorUtil(config: ScalaClientMethodConfig) {

  // TODO this would be a lot more maintainable as a method
  // defined on ScalaDatatype
  private def isList(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Container.List(_) => true
      case Datatype.Container.Option(inner) => isList(inner)
      case _ => false
    }
  }

  // TODO this would be a lot more maintainable as a method
  // defined on ScalaDatatype
  private def isMap(datatype: Datatype): Boolean = {
    datatype match {
      case Datatype.Container.Map(_) => true
      case _ => false
    }
  }

  // TODO this would be a lot more maintainable as a method
  // defined on ScalaDatatype
  private def isSingleValue(datatype: Datatype): Boolean = {
    !isList(datatype) && !isMap(datatype)
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
            case (p, dt) => s"""  Some("${p.originalName}" -> ${dt.asString(p.name)})"""
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

  def formBody(
    op: ScalaOperation,
    canSerializeUuid: Boolean
  ): Option[String] = {
    // Can have both or form params but not both as we can only send a single document
    assert(op.body.isEmpty || op.formParameters.isEmpty)

    def encodeValue(varName: String, dt: ScalaDatatype): String = dt match {
      case ScalaPrimitive.Uuid if !canSerializeUuid =>
        s"${varName}.toString"
      case _ => varName
    }

    if (op.formParameters.isEmpty && op.body.isEmpty) {
      None
    } else if (!op.body.isEmpty) {
      val body = op.body.get
      val payload = {
        val varName = body.datatype.toVariableName
        encodeValue(varName, body.datatype)
      }

      Some(s"val payload = play.api.libs.json.Json.toJson($payload)")

    } else {
      val params = op.formParameters.map { param =>
        val varName = ScalaUtil.quoteNameIfKeyword(param.name)
        val value = encodeValue(varName, param.datatype)
        s""""${param.originalName}" -> play.api.libs.json.Json.toJson(${value})"""
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

  object PathParamHelper {
    def urlEncode(
      name: String,
      d: ScalaDatatype
    ): String = {
      d match {
        case ScalaPrimitive.String => config.pathEncode(name)
        case ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => name
        case ScalaPrimitive.Enum(_, _) => config.pathEncode(s"$name.toString")
        case ScalaPrimitive.DateIso8601Joda | ScalaPrimitive.DateIso8601Java | ScalaPrimitive.DateTimeIso8601Java => s"$name.toString"
        case ScalaPrimitive.DateTimeIso8601Joda => config.pathEncode(s"_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($name)")
        case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) | ScalaPrimitive.ObjectAsPlay | ScalaPrimitive.ObjectAsCirce | ScalaPrimitive.Unit => {
          sys.error(s"Cannot encode params of type[$d] as path parameters (name: $name)")
        }
        case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encounteered as path param($name)")
      }
    }
  }

}
