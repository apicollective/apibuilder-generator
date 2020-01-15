package scala.models

import cats.implicits._
import lib.Datatype
import lib.generator.CodeGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import scala.generator._

object Play2Controllers extends CodeGenerator {

  def importJson(`import`: Import) =
    s"import ${`import`.namespace}.models.json._"

  def responseObjectCode(response: ScalaResponse) = response.code match {
    case ResponseCodeInt(code) => Some(code)
    case _ => None
  }

  def responseObjectName(response: ScalaResponse) = responseObjectCode(response).map(code => s"HTTP${code}")

  def responseObject(resource: ScalaResource, operation: ScalaOperation, response: ScalaResponse) = (responseObjectName(response), response.isUnit) match {
    case (Some(name), true) => Some(s"case object ${name} extends ${responsesTraitName(resource, operation)}")
    case (Some(name), false) => Some(s"case class ${name}(body: ${response.datatype.name}) extends ${responsesTraitName(resource, operation)}")
    case _ => None
  }

  def responsesTraitName(resource: ScalaResource, operation: ScalaOperation) = s"${resource.plural}${operation.name.capitalize}Responses"
  def responsesTraitAndObject(resource: ScalaResource, operation: ScalaOperation) =
    s"""
      trait ${responsesTraitName(resource, operation)}
      object ${responsesTraitName(resource, operation)} {
        ${operation.responses.flatMap(responseObject(resource, operation, _)).mkString("\n")}
      }
    """

  def serviceName(resource: ScalaResource) = s"${resource.plural}Service"
  def serviceMethod(resource: ScalaResource, operation: ScalaOperation) = {
    val parameters =
      List(s"""request: play.api.mvc.Request[${operation.body.fold("play.api.mvc.AnyContent")(_.datatype.name)}]""") ++
      operation.parameters.map(p => s"${p.name}: ${p.datatype.name}") ++
      operation.body.map(body => s"body: ${body.datatype.name}").toList

    s"""def ${operation.name}(${parameters.mkString(", ")}): scala.concurrent.Future[${responsesTraitName(resource, operation)}]"""
  }

  def service(resource: ScalaResource) =
    s"""
      trait ${serviceName(resource)} {
        ${resource.operations.map(serviceMethod(resource, _)).mkString("\n")}
      }
    """

  def controllerMethodResponse(resource: ScalaResource, operation: ScalaOperation, response: ScalaResponse) =
    (responseObjectName(response), responseObjectCode(response), response.isUnit) match {
      case (Some(name), Some(code), true) => Some(s"case ${responsesTraitName(resource, operation)}.${name} => Status(${code})(play.api.mvc.Results.EmptyContent())")
      case (Some(name), Some(code), false) => Some(s"case ${responsesTraitName(resource, operation)}.${name}(body) => Status(${code})(play.api.libs.json.Json.toJson(body))")
      case _ => None
    }

  def controllerMethod(resource: ScalaResource, operation: ScalaOperation) = {
    val bodyParser = operation.body.fold("")(body => s"(parse.json[${body.datatype.name}])")

    val parameters =
      List("request") ++
      operation.parameters.map(_.name) ++
      operation.body.map(_ => "request.body").toList

    s"""
      def ${operation.name}(${operation.parameters.map(p => s"${p.name}: ${p.datatype.name}").mkString(", ")}): play.api.mvc.Handler = Action.async${bodyParser} { request =>
        service.${operation.name}(${parameters.mkString(", ")}).map {
          ${operation.responses.flatMap(controllerMethodResponse(resource, operation, _)).mkString("\n")}
        }(executor)
      }
    """
  }

  def controller(resource: ScalaResource) =
    s"""
      @javax.inject.Singleton
      class ${resource.plural} @javax.inject.Inject()(
          service: ${serviceName(resource)},
          cc: play.api.mvc.ControllerComponents,
          executor: scala.concurrent.ExecutionContext,
        ) extends play.api.mvc.AbstractController(cc) {

        ${resource.operations.map(controllerMethod(resource, _)).mkString("\n")}

      }
    """

  def fileContents(form: InvocationForm, ssd: ScalaService): String =
    s"""
      ${ApidocComments(form.service.version, form.userAgent).toJavaString()}
      package controllers

      import ${ssd.namespaces.json}._
      ${ssd.service.imports.map(importJson).mkString("\n")}

      ${ssd.resources.flatMap(r => r.operations.map((r, _))).map { case (r, o) => responsesTraitAndObject(r, o) }.mkString("\n")}

      ${ssd.resources.map(service).mkString("\n")}

      ${ssd.resources.map(controller).mkString("\n")}
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
