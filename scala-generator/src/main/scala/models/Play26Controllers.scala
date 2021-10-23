package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.Datatype
import lib.generator.CodeGenerator

import scala.collection.mutable.ListBuffer
import scala.generator._

object Play26Controllers extends CodeGenerator {

  private[this] def importJson(`import`: Import): String = {
    s"import ${Namespaces.quote(`import`.namespace)}.models.json._"
  }

  private[this] def imports(ssd: ScalaService): String = {
    s"""
      import ${ssd.namespaces.json}._
      ${ssd.service.imports.map(importJson).mkString("\n")}
    """
  }

  private[this] def responseEnumName(operation: ScalaOperation) = s"${operation.name.capitalize}"

  private[this] def responseObjectCode(response: ScalaResponse): Option[Int] = response.code match {
    case ResponseCodeInt(code) => Some(code)
    case _ => None
  }

  private[this] def responseObjectName(response: ScalaResponse): Option[String] = responseObjectCode(response).map(code => s"HTTP$code")

  private[this] def overrideHeaders(responseConfig: ResponseConfig): Option[String] =  {
    responseConfig match {
      case ResponseConfig.Standard => None
      case ResponseConfig.Envelope => Some("override val headers: Seq[(String, String)] = Nil")
    }
  }


  private[this] case class ResponseObjectArgs(
    extendsClass: String,
    responseObjectName: String,
    responseDatatype: Datatype,
    responseConfig: ResponseConfig,
    code: Int,
  )

  private[this] def responseObject(responseObjectArgs: ResponseObjectArgs): String = {
    val args = ListBuffer[String]()
    if (responseObjectArgs.responseDatatype != Datatype.Primitive.Unit) {
      args.append(s"body: ${responseObjectArgs.responseDatatype.name}")
    }
    overrideHeaders(responseObjectArgs.responseConfig).foreach { code =>
      args.append(code)
    }

    CaseClassBuilder()
      .withName(responseObjectArgs.responseObjectName)
      .withExtendsClass(responseObjectArgs.extendsClass)
      .withArgList(Some(args.toSeq.mkString(",\n").indent(6)).filter(_.trim.nonEmpty))
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

  private[this] def responses(operation: ScalaOperation, response: ResponseConfig, responseTypes: Seq[ResponseObjectArgs]) = {
    val name = responseEnumName(operation)
    val headersCode = response match {
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

/*
  private[this] def responseToPlayOrig(operation: ScalaOperation, response: ScalaResponse): Option[String] =
    (responseObjectName(response), responseObjectCode(response), response.isUnit) match {
      case (Some(name), Some(code), true) => Some(s"case r: ${responseEnumName(operation)}.$name => Status($code)(play.api.mvc.Results.EmptyContent())")
      case (Some(name), Some(code), false) => Some(s"case r: ${responseEnumName(operation)}.$name => Status($code)(play.api.libs.json.Json.toJson(body))")
      case _ => None
    }
 */

  private[this] def responseToPlay(args: ResponseObjectArgs): String = {
    if (args.responseDatatype == Datatype.Primitive.Unit) {
      s"case ${args.extendsClass}.${args.responseObjectName} => Status(${args.code})(play.api.mvc.Results.EmptyContent())"
    } else {
      s"case r: ${args.extendsClass}.${args.responseObjectName} => Status(${args.code})(play.api.libs.json.Json.toJson(body))"
    }
  }

  private[this] def controllerMethod(operation: ScalaOperation, response: ResponseConfig): String = {
    val bodyType = operation.body.fold("play.api.mvc.AnyContent")(_.datatype.name)
    val bodyParser = operation.body.fold("")(body => s"(parse.json[${body.datatype.name}])")

    val parameterNames =
      List("request") ++
      operation.parameters.map(p => ScalaUtil.quoteNameIfKeyword(p.name)) ++
      operation.body.map(_ => "request.body").toList

    val parameterNameAndTypes =
      List(s"""request: play.api.mvc.Request[$bodyType]""") ++
      operation.parameters.map(p => s"${ScalaUtil.quoteNameIfKeyword(p.name)}: ${p.datatype.name}") ++
      operation.body.map(body => s"body: ${body.datatype.name}").toList

    val name = responseEnumName(operation)
    val responseTypes = operation.responses.flatMap { r =>
      responseObjectName(r).flatMap { n =>
        responseObjectCode(r).map { code =>
          ResponseObjectArgs(
            extendsClass = name,
            responseObjectName = n,
            responseDatatype = r.`type`,
            responseConfig = response,
            code = code,
          )
        }
      }
    } ++ Seq(
      ResponseObjectArgs(
        extendsClass = name,
        responseObjectName = "Undocumented",
        responseDatatype = Datatype.Primitive.Unit,
        responseConfig = response,
        code = 500,
      )
    )

    s"""
      ${responses(operation, response, responseTypes)}

      def ${operation.name}(${parameterNameAndTypes.mkString(", ")}): scala.concurrent.Future[${responseEnumName(operation)}]
      final def ${operation.name}(${operation.parameters.map(p => s"${ScalaUtil.quoteNameIfKeyword(p.name)}: ${p.datatype.name}").mkString(", ")}): play.api.mvc.Action[$bodyType] = Action.async$bodyParser { request =>
        ${operation.name}(${parameterNames.mkString(", ")})
          .map {
            ${responseTypes.map(responseToPlay).mkString("\n")}
            case ${responseEnumName(operation)}.Undocumented(result) => result
          }(defaultExecutionContext)
      }
    """
  }

  private[this] def controller(resource: ScalaResource, response: ResponseConfig) =
    s"""
      trait ${resource.plural}Controller extends play.api.mvc.BaseController {
        ${resource.operations.map { op => controllerMethod(op, response) }.mkString("\n\n")}
      }
    """

  private[this] def controllers(resources: Seq[ScalaResource], response: ResponseConfig): String = resources.map { r =>
    controller(r, response)
  }.mkString("\n\n")

  private[this] def fileContents(form: InvocationForm, ssd: ScalaService): String = {
    val attributeResponse = Attributes.PlayDefaultConfig.withAttributes(form.attributes).response
    println(s"Attributes.response: ${attributeResponse}")
    s"""
      ${ApiBuilderComments(form.service.version, form.userAgent).toJavaString}
      package ${ssd.namespaces.base}.controllers

      ${imports(ssd)}

      ${controllers(ssd.resources, attributeResponse)}
    """
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = ScalaService(form.service)
    val name = s"${ssd.name.split('-').map(_.capitalize).mkString}Controllers.scala"
    val contents = fileContents(form, ssd)

    utils.ScalaFormatter.format(contents) match {
      case Left(ex) => {
        println(s"contents: $contents")
        Left(Seq(
          s"Error formatting the generated code. This likely indicates a bug in the code generator. Error message: ${ex.getMessage}"
        ))
      }
      case Right(fmt) => Right(Seq(File(name, None, fmt, None)))
    }
  }
}
