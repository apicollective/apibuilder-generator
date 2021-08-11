package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.generator.CodeGenerator

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

  private[this] def responseObject(operation: ScalaOperation, response: ScalaResponse): Option[String] = (responseObjectName(response), response.isUnit) match {
    case (Some(name), true) => Some(s"case object $name extends ${responseEnumName(operation)}")
    case (Some(name), false) => Some(s"case class $name(body: ${response.datatype.name}) extends ${responseEnumName(operation)}")
    case _ => None
  }

  private[this] def responses(operation: ScalaOperation) =
    s"""
      sealed trait ${responseEnumName(operation)} extends Product with Serializable
      object ${responseEnumName(operation)} {
        ${operation.responses.flatMap(responseObject(operation, _)).mkString("\n")}
        case class Undocumented(result: play.api.mvc.Result) extends ${responseEnumName(operation)}
      }
    """

  private[this] def responseToPlay(operation: ScalaOperation, response: ScalaResponse): Option[String] =
    (responseObjectName(response), responseObjectCode(response), response.isUnit) match {
      case (Some(name), Some(code), true) => Some(s"case ${responseEnumName(operation)}.$name => Status($code)(play.api.mvc.Results.EmptyContent())")
      case (Some(name), Some(code), false) => Some(s"case ${responseEnumName(operation)}.$name(body) => Status($code)(play.api.libs.json.Json.toJson(body))")
      case _ => None
    }

  private[this] def controllerMethod(operation: ScalaOperation): String = {
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

    s"""
      ${responses(operation)}

      def ${operation.name}(${parameterNameAndTypes.mkString(", ")}): scala.concurrent.Future[${responseEnumName(operation)}]
      final def ${operation.name}(${operation.parameters.map(p => s"${ScalaUtil.quoteNameIfKeyword(p.name)}: ${p.datatype.name}").mkString(", ")}): play.api.mvc.Action[$bodyType] = Action.async$bodyParser { request =>
        ${operation.name}(${parameterNames.mkString(", ")})
          .map {
            ${operation.responses.flatMap(responseToPlay(operation, _)).mkString("\n")}
            case ${responseEnumName(operation)}.Undocumented(result) => result
          }(defaultExecutionContext)
      }
    """
  }

  private[this] def controller(resource: ScalaResource) =
    s"""
      trait ${resource.plural}Controller extends play.api.mvc.BaseController {
        ${resource.operations.map(controllerMethod).mkString("\n\n")}
      }
    """

  private[this] def controllers(resources: Seq[ScalaResource]): String = resources.map(controller).mkString("\n\n")

  private[this] def fileContents(form: InvocationForm, ssd: ScalaService): String =
    s"""
      ${ApiBuilderComments(form.service.version, form.userAgent).toJavaString}
      package ${ssd.namespaces.base}.controllers

      ${imports(ssd)}

      ${controllers(ssd.resources)}
    """

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = ScalaService(form.service)
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
