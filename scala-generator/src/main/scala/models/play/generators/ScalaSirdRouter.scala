// https://www.playframework.com/documentation/2.6.x/ScalaSirdRouter

package scala.models.play.generators

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Operation, ParameterLocation, Resource}
import lib.generator.{CodeGenerator, GeneratorUtil}
import scala.generator._

object ScalaSirdRouter extends CodeGenerator {

  def extractor(enum: ScalaEnum): String =
    s"""val ${enum.datatype.toVariableName} = new PathBindableExtractor[${enum.qualifiedName}]"""

  def handler(operation: ScalaOperation): String = {
    val name = operation.name
    val arguments = (operation.pathParameters ++ operation.queryParameters)
      .map { arg => s"${arg.name}: ${arg.datatype.name}" }

    s"""def ${name}(${arguments.mkString(", ")}): Handler"""
  }

  def route(operation: ScalaOperation): String = {
    def extractor(`type`: ScalaDatatype): Option[String] = `type` match {
      case ScalaDatatype.Option(inner) => extractor(inner)
      case ScalaPrimitive.Boolean => Some("bool")
      case ScalaPrimitive.Double => Some("double")
      case ScalaPrimitive.Integer => Some("int")
      case ScalaPrimitive.Long => Some("long")
      case enum: ScalaPrimitive.Enum => Some(s"Extractors.${enum.toVariableName}")
      case _ => None
    }

    val method = operation.method
    val path = operation.path
      .split("((?=/)|(?=\\.))")
      .map {
        case segment if !segment.startsWith("/:") => segment
        case segment =>
          operation.pathParameters.find(_.originalName == segment.drop(2)) match {
            case None => segment
            case Some(arg) =>
              val extractedArg = extractor(arg.datatype)
                .map { extractor => s"""${extractor}(${arg.name})""" }
                .getOrElse(arg.name)

              s"""/$${${extractedArg}}"""
          }
      }
      .mkString

    val query = {
      val arguments = operation.queryParameters
        .map { arg =>
          val interpolator = arg.datatype match {
            case _: ScalaDatatype.List => "q_s"
            case _: ScalaDatatype.Option => "q_o"
            case _ if arg.default.nonEmpty => "q_o"
            case _ => "q"
          }

          val extractedArg = extractor(arg.datatype)
            .map { extractor => s"""${extractor}(${arg.name})""" }
            .getOrElse(arg.name)

          s"""${interpolator}"${arg.originalName}=$${${extractedArg}}""""
        }

      arguments match {
        case Nil => ""
        case args => arguments.mkString(" ? ", " & ", "")
      }
    }

    val handlerName = operation.name
    val handlerArguments = (operation.pathParameters ++ operation.queryParameters)
      .map { arg =>
        arg.default match {
          case None => arg.name
          case Some(default) => s"""${arg.name}.getOrElse(${default})"""
        }
      }
      .mkString(", ")


    s"""case ${method}(p"${path}"${query}) => ${handlerName}(${handlerArguments})"""
  }

  def router(resource: ScalaResource): String = {
    val name = ScalaUtil.toClassName(resource.resource.`type`)
    val (handlers, routes) = resource.operations
      .map(o => (handler(o), route(o)))
      .unzip

    s"""
      |trait ${name}Routes extends SimpleRouter {
      |
      |  ${handlers.mkString("\n  ")}
      |
      |  override def routes: Routes = {
      |    ${routes.mkString("\n    ")}
      |  }
      |}
    """
  }

  def contents(service: ScalaService) = {
    val packagePrefix = service.namespaces.base
    val extractors = service.enums.map(extractor)
    val routers = service.resources.map(router)

    s"""
      |package ${packagePrefix}.routes
      |
      |import play.api.mvc.Handler
      |import play.api.routing.SimpleRouter
      |import play.api.routing.sird._
      |import play.api.routing.Router.Routes
      |
      |object Extractors {
      |  import io.flow.marketing.gateway.v0.Bindables.Models._
      |
      |  ${extractors.mkString("\n  ")}
      |}
      |
      |${routers.mkString("\n")}
    """.stripMargin.trim
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val scalaService = new ScalaService(form.service)
    val routesFile = File("Routes.scala", contents = contents(scalaService))

    Right(routesFile +: Nil)
  }
}
