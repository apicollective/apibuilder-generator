package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.Datatype
import lib.generator.CodeGenerator

import scala.generator._


object Play26Controllers extends PlayControllers(scalaVersion = ScalaVersion(2))
object Play29Scala3Controllers extends PlayControllers(scalaVersion = ScalaVersion(3))

abstract class PlayControllers(scalaVersion: ScalaVersion) extends CodeGenerator {

  private val BodyArgName: String = "body"

  private def importJson(`import`: Import): String = {
    s"import ${Namespaces.quote(`import`.namespace)}.models.json._"
  }

  private def imports(ssd: ScalaService): String = {
    s"""
      import ${ssd.namespaces.json}._
      ${ssd.service.imports.map(importJson).mkString("\n")}
    """
  }

  private def responseEnumName(operation: ScalaOperation) = s"${operation.name.capitalize}"

  private def responseObjectCode(response: ScalaResponse): Option[Int] = response.code match {
    case ResponseCodeInt(code) => Some(code)
    case _ => None
  }

  private def responseObjectName(response: ScalaResponse): Option[String] = responseObjectCode(response).map(code => s"HTTP$code")

  private def overrideHeaders(responseConfig: ResponseConfig): Option[CaseClassArgument] =  {
    responseConfig match {
      case ResponseConfig.Standard => None
      case ResponseConfig.Envelope => Some(
        CaseClassArgument(
          name = "headers",
          `type` = "Seq[(String, String)]",
          modifiers = Seq("override", "val"),
          defaultValue = Some("Nil"),
        )
      )
    }
  }


  private case class CaseClassArgument(
    name: String,
    `type`: String,
    modifiers: Seq[String] = Nil,
    defaultValue: Option[String] = None,
  ) {
    def declaration: String = {
      val base = (modifiers.mkString(" ").strip + s" ${name}: ${`type`}").strip
      defaultValue match {
        case None => base
        case Some(dv) => s"$base = $dv"
      }
    }
  }

  private case class ResponseObjectArgs(
    extendsClass: String,
    bodyArg: Option[CaseClassArgument],
    headerArg: Option[CaseClassArgument],
    responseArg: Option[CaseClassArgument] = None,
    responseObjectName: String,
    responseDatatype: String,
    responseConfig: ResponseConfig,
    code: Int,
  ) {
    assert(
      bodyArg.isEmpty || responseArg.isEmpty,
      "Cannot declare both a body and a response"
    )
    val allArgs: Seq[CaseClassArgument] = bodyArg.toSeq ++ headerArg.toSeq ++ responseArg.toSeq
  }

  private def responseObject(responseObjectArgs: ResponseObjectArgs): String = {
    CaseClassBuilder()
      .withName(responseObjectArgs.responseObjectName)
      .withExtendsClass(responseObjectArgs.extendsClass)
      .withArgList(Some(
        responseObjectArgs.allArgs.map(_.declaration).mkString(",\n")
          .indent(6)).filter(_.trim.nonEmpty)
      )
      .withBodyParts(
        responseObjectArgs.responseConfig match {
          case ResponseConfig.Standard => Nil
          case ResponseConfig.Envelope => Seq(
            s"def withHeaders(headers: Seq[(String, String)]): ${responseObjectArgs.responseObjectName} = this.copy(headers = headers)"
          )
        }
      )
      .build
  }

  private def responses(operation: ScalaOperation, responseConfig: ResponseConfig, responseTypes: Seq[ResponseObjectArgs]) = {
    val name = responseEnumName(operation)
    val headersCode = responseConfig match {
      case ResponseConfig.Standard => ""
      case ResponseConfig.Envelope => " " +
        s""" {
           |  def headers: Seq[(String, String)] = Nil
           |}
           |""".stripMargin.strip
    }

    s"""
      sealed trait $name extends Product with Serializable$headersCode
      object $name {
        ${responseTypes.map(responseObject).mkString("\n")}
      }
    """
  }

  private def responseToPlay(obj: ResponseObjectArgs): String = {
    if (obj.allArgs.isEmpty) {
      s"case ${obj.extendsClass}.${obj.responseObjectName} => Status(${obj.code})(play.api.mvc.Results.EmptyContent())"
    } else {
      def withHeaders(code: String) = {
        val base = s"case r: ${obj.extendsClass}.${obj.responseObjectName} => $code"
        if (obj.headerArg.isEmpty) {
          base
        } else {
          s"$base.withHeaders(r.headers${scalaVersion.splat})"
        }
      }

      def withStatus(msg: String) = withHeaders(s"Status(${obj.code})($msg)")

      (obj.bodyArg, obj.responseArg) match {
        case (Some(_), Some(_)) => sys.error("Case class arguments cannot declare both a body and a response")
        case (Some(body), None) => withStatus(s"play.api.libs.json.Json.toJson(r.${body.name})")
        case (None, Some(res)) => withHeaders(s"r.${res.name}")
        case (None, None) => withStatus("play.api.mvc.Results.EmptyContent()")
      }
    }
  }

  private def controllerMethod(operation: ScalaOperation, responseConfig: ResponseConfig): String = {
    val bodyType = operation.body.fold("play.api.mvc.AnyContent")(_.datatype.name)
    val bodyParser = operation.body.fold("")(body => s"(parse.json[${body.datatype.name}])")

    val parameterNames =
      List("request") ++
      operation.parameters.map(p => ScalaUtil.quoteNameIfKeyword(p.name)) ++
      operation.body.map(_ => s"request.${BodyArgName}").toList

    val parameterNameAndTypes =
      List(s"""request: play.api.mvc.Request[$bodyType]""") ++
      operation.parameters.map(p => s"${ScalaUtil.quoteNameIfKeyword(p.name)}: ${p.datatype.name}") ++
      operation.body.map(body => s"$BodyArgName: ${body.datatype.name}").toList

    val name = responseEnumName(operation)
    val headerArg = overrideHeaders(responseConfig)
    val responseTypes = operation.responses.flatMap { r =>
      responseObjectName(r).flatMap { n =>
        responseObjectCode(r).map { code =>
          val bodyArg = if (r.isUnit) {
            None
          } else {
            Some(CaseClassArgument(BodyArgName, r.datatype.name))
          }

          ResponseObjectArgs(
            extendsClass = name,
            bodyArg = bodyArg,
            headerArg = headerArg,
            responseObjectName = n,
            responseDatatype = r.datatype.name,
            responseConfig = responseConfig,
            code = code,
          )
        }
      }
    } ++ Seq(
      ResponseObjectArgs(
        extendsClass = name,
        bodyArg = None,
        headerArg = headerArg,
        responseArg = Option(
          CaseClassArgument(
            name = "result",
            `type` = "play.api.mvc.Result"
          ),
        ),
        responseObjectName = "Undocumented",
        responseDatatype = Datatype.Primitive.Unit.name,
        responseConfig = responseConfig,
        code = 500,
      )
    )

    s"""
      ${responses(operation, responseConfig, responseTypes)}

      def ${operation.name}(${parameterNameAndTypes.mkString(", ")}): scala.concurrent.Future[${responseEnumName(operation)}]
      final def ${operation.name}(${operation.parameters.map(p => s"${ScalaUtil.quoteNameIfKeyword(p.name)}: ${p.datatype.name}").mkString(", ")}): play.api.mvc.Action[$bodyType] = Action.async$bodyParser { request =>
        ${operation.name}(${parameterNames.mkString(", ")})
          .map {
            ${responseTypes.map(responseToPlay).mkString("\n")}
          }(defaultExecutionContext)
      }
    """
  }

  private def controller(resource: ScalaResource, responseConfig: ResponseConfig) =
    s"""
      trait ${resource.plural}Controller extends play.api.mvc.BaseController {
        ${resource.operations.map { op => controllerMethod(op, responseConfig) }.mkString("\n\n")}
      }
    """

  private def controllers(resources: Seq[ScalaResource], responseConfig: ResponseConfig): String = resources.map { r =>
    controller(r, responseConfig)
  }.mkString("\n\n")

  private def fileContents(form: InvocationForm, ssd: ScalaService): String = {
    val attributeResponse = Attributes.PlayDefaultConfig.withAttributes(form.attributes).response
    s"""
      ${ApiBuilderComments(form.service.version, form.userAgent).toJavaString}
      package ${ssd.namespaces.base}.controllers

      ${imports(ssd)}

      ${controllers(ssd.resources, attributeResponse)}
    """
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
    val name = s"${ssd.name.split('-').map(_.capitalize).mkString}Controllers.scala"
    val contents = fileContents(form, ssd)

    utils.ScalaFormatter.format(contents) match {
      case Left(ex) => {
        Left(Seq(
          s"Error formatting the generated code. This likely indicates a bug in the code generator. Error message: ${ex.getMessage}"
        ))
      }
      case Right(fmt) => Right(Seq(File(name, None, fmt, None)))
    }
  }
}
