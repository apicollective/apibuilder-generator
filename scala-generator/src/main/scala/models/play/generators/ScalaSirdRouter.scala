// https://www.playframework.com/documentation/2.6.x/ScalaSirdRouter

package scala.models.play.generators


import cats.data._
import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Operation, ParameterLocation, Resource}
import lib.generator.{CodeGenerator, GeneratorUtil}
import scala.generator._

object ScalaSirdRouter extends CodeGenerator {

  def handler(operation: ScalaOperation): String = {
    val name = operation.name
    val arguments = (operation.pathParameters ++ operation.queryParameters)
      .map { arg => s"${arg.name}: ${arg.datatype.name}" }

    s"""def ${name}(${arguments.mkString(", ")}): Handler"""
  }

  def extractor(`type`: ScalaDatatype): ValidatedNel[String, Option[String]] = `type` match {
    case ScalaDatatype.Option(inner) => extractor(inner)
    case ScalaDatatype.List(inner) => extractor(inner)
    case ScalaPrimitive.String => Validated.validNel(none[String])
    case ScalaPrimitive.Boolean => Validated.validNel("bool".some)
    case ScalaPrimitive.Double => Validated.validNel("double".some)
    case ScalaPrimitive.Integer => Validated.validNel("int".some)
    case ScalaPrimitive.Long => Validated.validNel("long".some)
    case ScalaPrimitive.DateTimeIso8601Joda => Validated.validNel("Bindables.Core.pathBindableExtractorDateTimeIso8601".some)
    case ScalaPrimitive.DateIso8601Joda => Validated.validNel("Bindables.Core.pathBindableExtractorDateIso8601".some)
    case enum: ScalaPrimitive.Enum => Validated.validNel(s"Bindables.Models.pathBindableExtractor${enum.shortName}".some)
    case _ => Validated.invalidNel(s"No extractor defined for '${`type`}')")
  }

  def queryInterpolator(arg: ScalaParameter): String = arg.datatype match {
    case _: ScalaDatatype.List => "q_s"
    case _: ScalaDatatype.Option => "q_o"
    case _ if arg.default.nonEmpty => "q_o"
    case _ => "q"
  }

  def path(operation: ScalaOperation): ValidatedNel[String, String] =
    operation.path
      .split("((?=/)|(?=\\.))")
      .toList
      .traverse {
        case segment if(!segment.startsWith("/:")) => Validated.validNel(segment)
        case segment =>
          operation.pathParameters
            .find(_.originalName == segment.drop(2))
            .toValidNel(s"Missing path parameter '${segment.drop(2)}'")
            .andThen { arg =>
              extractor(arg.datatype)
                .map {
                  case Some(extractor) => s"""${extractor}(${arg.name})"""
                  case None => arg.name
                }
                .map { extractorAndArgument =>
                  s"""/$${${extractorAndArgument}}"""
                }
            }
      }
      .map(_.mkString)

  def query(operation: ScalaOperation): ValidatedNel[String, String] =
    operation.queryParameters
      .map { arg => (arg, queryInterpolator(arg)) }
      .traverse { case (arg, interpolator) =>
        extractor(arg.datatype)
          .map {
            case Some(extractor) => s"""${extractor}(${arg.name})"""
            case None => arg.name
          }
          .map { extractorAndArgument =>
            s"""${interpolator}"${arg.originalName}=$${${extractorAndArgument}}""""
          }
      }
      .map {
        case Nil => ""
        case args => args.mkString(" ? ", " & ", "")
      }

  def route(operation: ScalaOperation): ValidatedNel[String, String] =
    (path(operation), query(operation))
      .mapN { case (path, query) =>
        val method = operation.method
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

  def router(resource: ScalaResource): ValidatedNel[String, String] =
    for {
      routes <- resource.operations.toList.traverse(route)
      handlers = resource.operations.map(handler)
      name = ScalaUtil.toClassName(resource.resource.`type`)

    } yield s"""
        |trait ${name}Routes extends SimpleRouter {
        |
        |  ${handlers.mkString("\n  ")}
        |
        |  override def routes: Routes = {
        |    ${routes.mkString("\n    ")}
        |  }
        |}
      """

  def contents(service: ScalaService): ValidatedNel[String, String] =
    service.resources.toList.traverse(router)
      .map { routers =>
        val bindables = scala.models.Play2Bindables(service).build
        val packagePrefix = service.namespaces.base

        s"""
          |package ${packagePrefix}.routes
          |
          |import play.api.mvc.Handler
          |import play.api.routing.SimpleRouter
          |import play.api.routing.sird._
          |import play.api.routing.Router.Routes
          |
          |${bindables}
          |
          |${routers.mkString("\n")}
        """
      }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val scalaService = new ScalaService(form.service)
    contents(scalaService)
      .map { contents =>
        Seq(File("Routes.scala", contents = contents.stripMargin.trim))
      }
      .toEither
      .leftMap(_.toList)
  }
}
