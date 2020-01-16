package scala.models

import cats.implicits._
import lib.Datatype
import lib.generator.CodeGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import scala.generator._

object Play26Controllers extends CodeGenerator {

  def importJson(`import`: Import) =
    s"import ${`import`.namespace}.models.json._"

  def imports(ssd: ScalaService) =
    s"""
      import ${ssd.namespaces.json}._
      ${ssd.service.imports.map(importJson).mkString("\n")}
    """

  def responseEnumName(operation: ScalaOperation) = s"${operation.name.capitalize}"

  def responseObjectCode(response: ScalaResponse) = response.code match {
    case ResponseCodeInt(code) => Some(code)
    case _ => None
  }

  def responseObjectName(response: ScalaResponse) = responseObjectCode(response).map(code => s"HTTP${code}")

  def responseObject(operation: ScalaOperation, response: ScalaResponse) = (responseObjectName(response), response.isUnit) match {
    case (Some(name), true) => Some(s"case object ${name} extends ${responseEnumName(operation)}")
    case (Some(name), false) => Some(s"case class ${name}(body: ${response.datatype.name}) extends ${responseEnumName(operation)}")
    case _ => None
  }

  def responses(operation: ScalaOperation) =
    s"""
      sealed trait ${responseEnumName(operation)} extends Product with Serializable
      object ${responseEnumName(operation)} {
        ${operation.responses.flatMap(responseObject(operation, _)).mkString("\n")}
      }
    """

  def responseToPlay(operation: ScalaOperation, response: ScalaResponse) =
    (responseObjectName(response), responseObjectCode(response), response.isUnit) match {
      case (Some(name), Some(code), true) => Some(s"case ${responseEnumName(operation)}.${name} => Status(${code})(play.api.mvc.Results.EmptyContent())")
      case (Some(name), Some(code), false) => Some(s"case ${responseEnumName(operation)}.${name}(body) => Status(${code})(play.api.libs.json.Json.toJson(body))")
      case _ => None
    }

  def controllerMethod(operation: ScalaOperation) = {
    val bodyParser = operation.body.fold("")(body => s"(parse.json[${body.datatype.name}])")

    val parameterNames =
      List("request") ++
      operation.parameters.map(_.name) ++
      operation.body.map(_ => "request.body").toList

    val parameterNameAndTypes =
      List(s"""request: play.api.mvc.Request[${operation.body.fold("play.api.mvc.AnyContent")(_.datatype.name)}]""") ++
      operation.parameters.map(p => s"${p.name}: ${p.datatype.name}") ++
      operation.body.map(body => s"body: ${body.datatype.name}").toList

    s"""
      ${responses(operation)}

      def ${operation.name}(${parameterNameAndTypes.mkString(", ")}): scala.concurrent.Future[${responseEnumName(operation)}]
      final def ${operation.name}(${operation.parameters.map(p => s"${p.name}: ${p.datatype.name}").mkString(", ")}): play.api.mvc.Handler = Action.async${bodyParser} { request =>
        ${operation.name}(${parameterNames.mkString(", ")})
          .map {
            ${operation.responses.flatMap(responseToPlay(operation, _)).mkString("\n")}
          }(defaultExecutionContext)
      }
    """
  }

  def controller(resource: ScalaResource) =
    s"""
      trait ${resource.plural}Controller extends play.api.mvc.BaseController {
        ${resource.operations.map(controllerMethod).mkString("\n")}
      }
    """

  def controllers(resources: Seq[ScalaResource]) = resources.map(controller).mkString("\n\n")

  def fileContents(form: InvocationForm, ssd: ScalaService): String =
    s"""
      ${ApidocComments(form.service.version, form.userAgent).toJavaString()}
      package ${ssd.namespaces.base}.controllers

      ${imports(ssd)}

      ${controllers(ssd.resources)}
    """

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = ScalaService(form.service)
    val name = s"${ssd.name.split('-').map(_.capitalize).mkString}Controllers.scala"
    val contents = fileContents(form, ssd)

    utils.ScalaFormatter.format(contents)
      .map(contents => Seq(File(name, None, contents, None)))
      .leftMap(t => Seq(t.getMessage))
  }
}
