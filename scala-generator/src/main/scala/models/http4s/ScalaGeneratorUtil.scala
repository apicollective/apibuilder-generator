package scala.models.http4s

import scala.generator.{ScalaPrimitive, ScalaClientMethodConfig, ScalaUtil, ScalaOperation, ScalaDatatype}

import io.apibuilder.spec.v0.models.ParameterLocation
import lib.Text._

class ScalaGeneratorUtil(config: ScalaClientMethodConfig) extends scala.generator.ScalaGeneratorUtil(config) {
  override def pathParams(op: ScalaOperation): String = {
    val pairs = op.pathParameters.map { p =>
      require(p.location == ParameterLocation.Path, "Only singletons can be path parameters.")
      s":${p.originalName}" -> urlEncode(p.asScalaVal, p.datatype)
    }.toMap
    val path = op.path.split("/").map(_.trim).filter(!_.isEmpty).map { e => pairs.get(e).getOrElse(s""""${e.takeWhile(_ != '#')}"""") }
    path.mkString("Seq(", ", ", ")").trim
  }

  override def formBody(
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

      Some(s"val payload = $payload")

    } else {
      val params = op.formParameters.map { param =>
        val value = encodeValue(param.asScalaVal, param.datatype)
        s""""${param.originalName}" -> ${value}.asJson.noSpaces"""
      }.mkString(",\n")
      Some(
        Seq(
          "val payload = { import io.circe.syntax._ ; org.http4s.UrlForm(",
          params.indent,
          ") }"
        ).mkString("\n")
      )
    }
  }

  private def urlEncode( name: String, d: ScalaDatatype): String = d match {
    case ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid => config.pathEncode(s"$name.toString")
    case _ => PathParamHelper.urlEncode(name, d)
  }
}
