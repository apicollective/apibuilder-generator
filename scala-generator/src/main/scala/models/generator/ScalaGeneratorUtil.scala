package scala.generator

import com.bryzek.apidoc.spec.v0.models.ParameterLocation
import lib.Datatype
import lib.Text._

case class ScalaGeneratorUtil(config: ScalaClientMethodConfig) {

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
        case ScalaPrimitive.DateTimeIso8601 => s"""${config.pathEncodingMethod}(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print($name), "UTF-8")"""
        case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) | ScalaPrimitive.Object | ScalaPrimitive.Unit => {
          sys.error(s"Cannot encode params of type[$d] as path parameters (name: $name)")
        }
        case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encounteered as path param($name)")
      }
    }
  }

}
