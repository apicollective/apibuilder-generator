package scala.models.http4s.server

import io.apibuilder.spec.v0.models.ResponseCodeInt

import scala.generator.{ScalaClientMethodConfigs, ScalaDatatype, ScalaOperation, ScalaParameter, ScalaPrimitive, ScalaResource, ScalaUtil}
import lib.Text._

import scala.models.http4s.ScalaService

sealed trait PathSegment
case class Literal(name: String) extends PathSegment
case class PlainString(name: String) extends PathSegment
case class Extracted(name: String, parameter: ScalaParameter) extends PathSegment

case class StatusCode(code: Int, datatype: Option[String])

case class Route(ssd: ScalaService, resource: ScalaResource, op: ScalaOperation, config: ScalaClientMethodConfigs.Http4s) {
  val pathSegments: Array[PathSegment] = op.path.split("/").filterNot(_.isEmpty).map { segment =>
    if(!segment.startsWith(":")) {
      Literal(segment)
    } else {
      val truncated = segment.drop(1) //ScalaUtil.toVariable(segment.drop(1))
      op.pathParameters.find(_.originalName == truncated).fold(Literal(segment): PathSegment) { scalaParameter =>
        scalaParameter.datatype match {
          case ScalaPrimitive.String if scalaParameter.param.minimum.isEmpty && scalaParameter.param.maximum.isEmpty => PlainString(truncated)
          case _: ScalaPrimitive => Extracted(truncated, scalaParameter)
        }
      }
    }
  }

  val responseTrait = s"${op.name.capitalize}Response"

  val statusCodes: Seq[StatusCode] = {
    op.responses.flatMap { response =>
      response.code  match {
        case ResponseCodeInt(value)  if HttpStatusCodes.contains(value) =>
          val returnType = if (response.isUnit) {
            None
          } else {
            Some(response.datatype.name)
          }
          Some(StatusCode(value, returnType))
        case _ => None
      }
    }
  }

  def operation(): Seq[String] = {
    val responses = statusCodes.map { case StatusCode(code, datatype) =>
      datatype.fold(
        s"""case class HTTP$code(headers: Seq[org.http4s.Header] = Nil) extends $responseTrait""".stripMargin
      )(typ =>
        s"""case class HTTP$code(value: $typ, headers: Seq[org.http4s.Header] = Nil) extends $responseTrait""".stripMargin
      )
    }

    val params =
      Seq(Some(s"_req: ${config.requestClass}")) ++
      op.nonHeaderParameters.map { field =>
        val typ = field.datatype match {
          case ScalaDatatype.Option(container: ScalaDatatype.Container) => container.name
          case other => other.name
        }
        Some(s"${ScalaUtil.quoteNameIfKeyword(field.name)}: $typ")
      } ++
      Seq(op.body.map(body => s"body: => ${config.generateDecodeResult(body.datatype.name)}"))

    Seq(
      s"sealed trait $responseTrait",
      s"",
      s"object ${op.name.capitalize}Response {",
      s"${responses.mkString("\n").indent(2)}",
      s"}",
      s"",
      s"def ${op.name}(",
      params.flatten.mkString(",\n").indent(2),
      s"): ${config.asyncType}[$responseTrait]"
    )
  }

  def route(version: Option[Int]): Seq[String] = {
    val args = (
      Seq(Some("_req")) ++
        op.nonHeaderParameters.map(field => Some(s"${ScalaUtil.quoteNameIfKeyword(field.originalName)}")) ++
        Seq(op.body.map(body => s"_req.attemptAs[${body.datatype.name}]"))
      ).flatten.mkString(", ")

    val path = (
      Seq("Root") ++
        pathSegments.collect {
          case Literal(n) => s""""$n""""
          case PlainString(n) => s"$n"
          case Extracted(name, param) => s"${Http4sServer.pathExtractor(ssd, param).name}($name)"
        }
      ).mkString(" / ")

    val query = (
      op.queryParameters.map { param =>
        val extractor = Http4sServer.queryExtractor(ssd, param)
        s"${extractor.name}(${extractor.handler})"
      }
      ).mkString(" +& ")

    val queryStart = if (query.size > 0) """ :? """ else ""

    val verFilter = version.fold("")(_ => " if apiVersionMatch(_req)")

    Seq(
      s"case _req @ ${op.method} -> $path$queryStart$query$verFilter =>",
      s"  ${op.name}($args).flatMap {"
    ) ++ statusCodes.collect {
      case StatusCode(code, Some(_)) =>
        s"    case $responseTrait.HTTP$code(value, headers) => ${HttpStatusCodes.apply(code)}(value).putHeaders(headers: _*)"
      case StatusCode(code, None) =>
        s"    case $responseTrait.HTTP$code(headers) => ${HttpStatusCodes.apply(code)}().putHeaders(headers: _*)"
    } ++ Seq(
      s"  }"
    )
  }
}
