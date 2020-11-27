package scala.models.http4s.server

import io.apibuilder.spec.v0.models.ResponseCodeInt
import lib.Text._

import scala.generator._
import scala.models.http4s.ScalaService

sealed trait PathSegment
case class Literal(name: String) extends PathSegment
case class PlainString(name: String) extends PathSegment
case class Extracted(parameter: ScalaParameter) extends PathSegment

case class Route(ssd: ScalaService, resource: ScalaResource, op: ScalaOperation, config: ScalaClientMethodConfigs.Http4s) {
  val pathSegments: Array[PathSegment] = op.path.split("/").filterNot(_.isEmpty).map { segment =>
    if(!segment.startsWith(":")) {
      Literal(segment)
    } else {
      val truncated = segment.drop(1) //ScalaUtil.toVariable(segment.drop(1))
      op.pathParameters.find(_.originalName == truncated).fold(Literal(segment): PathSegment) { scalaParameter =>
        scalaParameter.datatype match {
          case ScalaPrimitive.String if scalaParameter.param.minimum.isEmpty && scalaParameter.param.maximum.isEmpty => PlainString(scalaParameter.asScalaVal)
          case _: ScalaPrimitive => Extracted(scalaParameter)
          case _: ScalaDatatype => Extracted(scalaParameter)
        }
      }
    }
  }

  val responseTrait = s"${op.name.capitalize}Response"

  val statusCodes: Seq[StatusCode] = {
    op.responses.flatMap { response =>
      response.code  match {
        case ResponseCodeInt(value) =>
          HttpStatusCodes(value).map { status =>
            if (response.isUnit) {
              status
            } else {
              status.withBodyType(response.datatype.name)
            }
          }
        case _ => None
      }
    }
  }

  val nonHeaderParameters = op.nonHeaderParameters.map { field =>
    val typ = field.datatype match {
      case ScalaDatatype.Option(container: ScalaDatatype.Container) => container.name
      case other => other.name
    }
    Some(s"${field.asScalaVal}: $typ")
  }

  val requestCaseClassName = s"${op.name.capitalize}Request"

  def operation(): Seq[String] = {

    val responses = statusCodes.map { s =>
      s"""case class HTTP${s.code}(${s.responseParams(config)}) extends $responseTrait""".stripMargin
    } ++ Seq(
      s"""case class UndocumentedResponse(response: ${config.asyncType}[${config.responseClass}]) extends $responseTrait""".stripMargin
    )

    val params =
      Seq(Some(s"_req: ${config.requestClass}")) ++
      nonHeaderParameters ++
      Seq(op.body.map(body => s"body: => ${config.generateDecodeResult(body.datatype.name)}"))

    val requestCaseClass = s"$requestCaseClassName(${nonHeaderParameters.flatten.mkString(", ")})"

    val requestDecoder = Seq(s"implicit val ${requestCaseClassName}Decoder: _root_.io.circe.Decoder[$requestCaseClassName] = _root_.io.circe.Decoder.instance { a =>",
                             s"  for {",
                             op.nonHeaderParameters.map(p => s"""    ${p.asScalaVal} <- a.downField("${p.originalName}").as[${p.datatype.name}]""").mkString("\n"),
                             s"  } yield {",
                             s"    $requestCaseClassName(",
                             op.nonHeaderParameters.map(p => s"""      ${p.asScalaVal} = ${p.asScalaVal}""").mkString(",\n"),
                             s"    )",
                             s"  }",
                             s"}")

    val requestCaseClassAndDecoder = op.formParameters match {
      case Nil => Seq()
      case _ => Seq(
        s"case class $requestCaseClass",
        s"") ++ requestDecoder ++ Seq("")
    }

    requestCaseClassAndDecoder ++
    Seq(
      s"sealed trait $responseTrait",
      s"",
      s"object ${op.name.capitalize}Response {",
      s"${responses.mkString("\n").indentString(2)}",
      s"}",
      s"",
      s"def ${op.name}(",
      params.flatten.mkString(",\n").indentString(2),
      s"): ${config.asyncType}[$responseTrait]"
    )
  }

  def route(version: Option[Int]): Seq[String] = {

    def nonHeaderParameters(prefix:String) = op.nonHeaderParameters.map(field => Some(s"$prefix${field.asScalaVal}"))

    def args(prefix:String) = (
      Seq(Some("_req")) ++
        nonHeaderParameters(prefix) ++
        Seq(op.body.map(body => s"_req.attemptAs[${body.datatype.name}]"))
      ).flatten.mkString(", ")

    val path = (
      Seq("Root") ++
        pathSegments.collect {
          case Literal(n) => s""""$n""""
          case PlainString(n) => s"$n"
          case Extracted(param) => s"${Http4sServer.pathExtractor(ssd, param).name}(${param.asScalaVal})"
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

    def route(prefix:String) = {
      Seq(s"  ${op.name}(${args(prefix)}).flatMap {") ++
      statusCodes.map { case s =>
        s"    case $responseTrait.HTTP${s.code}(${s.responseExtractor(config)}) => ${s.name}(${s.applyArgs(config)})"
      } ++
      Seq(
        s"    case $responseTrait.UndocumentedResponse(response) => response",
        s"  }"
      )
    }

    def prefix(filter: String) = s"case _req @ ${op.method} -> $path$queryStart$query$filter =>"

    val verFilterPrefix = Seq(prefix(verFilter))

    val decodingParameters:Seq[String] = {
      op.nonHeaderParameters.map { field =>
        val name = field.asScalaVal
        val originalName = ScalaUtil.quoteNameIfKeyword(field.originalName)
        val datatype = field.datatype.name

        field.datatype match {
          case ScalaPrimitive.String => s"""$name <- req.getFirst("$originalName")"""
          case _: ScalaPrimitive => s"""$name <- req.getFirst("$originalName").flatMap(f => _root_.io.circe.parser.decode[$datatype](f).toOption)"""
          case ScalaDatatype.Option(inner) =>
            inner match {
              case ScalaPrimitive.String => s"""$name <- Some(req.getFirst("$originalName"))"""
              case _ => s"""$name <- Some(req.getFirst("$originalName").flatMap(f => _root_.io.circe.parser.decode[${inner.name}](f).toOption))"""
            }
          case ScalaDatatype.Map(_) =>
            s"""$name <- req.getFirst("$originalName").flatMap(f => _root_.io.circe.parser.decode[$datatype](f).toOption)"""
          case ScalaDatatype.List(inner) =>
            inner match {
              case ScalaPrimitive.String => s"""$name <- Some(req.get("$originalName"))"""
              case _ => s"""$name <- Some(req.get("$originalName").flatMap(f => _root_.io.circe.parser.decode[${inner.name}](f).toOption))"""
            }
        }
      }.map(_.indentString(10))
    }

    val decoding = if(op.formParameters.nonEmpty) {
      Seq(s"if (_req.contentType.exists(_.mediaType == ${config.applicationJsonMediaType})) {",
          s"  _req.attemptAs[$requestCaseClassName].value.flatMap{",
          s"    case Right(req) =>") ++ route("req.").map(_.indentString(4)) ++
      Seq(s"    case Left(_) => BadRequest()",
          s"  }",
          s"} else {",
          s"    _req.decode[_root_.org.http4s.UrlForm] {",
          s"      req =>",
          s"        val responseOpt = for {") ++ decodingParameters ++
        Seq(s"        } yield {") ++ route("").map(_.indentString(10)) ++
        Seq(s"          }",
          s"        responseOpt.getOrElse(BadRequest())",
          s"  }",
          s"}")
    } else {
      route("")
    }

    val missingVersionHeaderCase = version.fold(Seq(""))(_ =>
                                                  Seq(prefix(s" if !_req.headers.get(ApiVersion.ApiVersionMajor).isDefined") ++ "\n" ++
                                                    s"""  BadRequest(s"Missing required request header: $${ApiVersion.ApiVersionMajor}.")"""))

    verFilterPrefix ++ decoding ++ missingVersionHeaderCase

  }

}
