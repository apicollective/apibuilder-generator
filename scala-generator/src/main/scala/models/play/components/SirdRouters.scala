package scala.models.play.components

import cats.data._
import io.apibuilder.spec.v0.models._
import scala.generator._

case class RouterHandler(operation: ScalaOperation) {
  def syntax: String = {
    val name = operation.name
    val arguments = (operation.pathParameters ++ operation.queryParameters)
      .map { arg => s"${arg.asScalaVal}: ${arg.datatype.name}" }

    s"""def ${name}(${arguments.mkString(", ")}): _root_.play.api.mvc.Handler"""
  }
}

case class RouterPatternMatch(operation: ScalaOperation, bindablesPackage: String) {
  def variableName(arg: ScalaParameter, index: Int) = s"_${index}"

  def extractor(arg: ScalaParameter, index: Int): String = {
    @annotation.tailrec
    def recursive(tpe: ScalaDatatype): Option[String] = tpe match {
      case ScalaDatatype.Option(inner) => recursive(inner)
      case ScalaDatatype.List(inner) => recursive(inner)
      case ScalaPrimitive.Boolean => Some("bool")
      case ScalaPrimitive.Double => Some("double")
      case ScalaPrimitive.Integer => Some("int")
      case ScalaPrimitive.Long => Some("long")
      case ScalaPrimitive.DateTimeIso8601Joda => Some(s"${bindablesPackage}.Core.pathBindableExtractorDateTimeIso8601")
      case ScalaPrimitive.DateIso8601Joda => Some(s"${bindablesPackage}.Core.pathBindableExtractorDateIso8601")
      case ScalaPrimitive.Uuid => Some(s"${bindablesPackage}.Core.pathBindableExtractorUUID")
      case enum: ScalaPrimitive.Enum => Some(s"${enum.namespaces.bindables}.Models.pathBindableExtractor${enum.shortName}")
      case _ => None
    }

    val name = variableName(arg, index)
    recursive(arg.datatype) match {
      case Some(extractor) => s"""${extractor}(${name})"""
      case None => name
    }
  }

  def queryInterpolator(arg: ScalaParameter): String = arg.datatype match {
    case ScalaDatatype.List(_) => "q_s"
    case ScalaDatatype.Option(inner) if inner.isInstanceOf[ScalaDatatype.List] => "q_s"
    case ScalaDatatype.Option(_) => "q_o"
    case _ if arg.default.nonEmpty => "q_o"
    case _ => "q"
  }

  def getter(arg: ScalaParameter, index: Int): String = {
    val name = variableName(arg, index)
    arg.datatype match {
      case ScalaDatatype.Option(inner) if inner.isInstanceOf[ScalaDatatype.List] => s"if(${name}.nonEmpty) Some(${name}) else None"
      case ScalaDatatype.List(_) => name
      case _ => arg.default.fold(name) { default => s"""${name}.getOrElse(${default})""" }
    }
  }

  def path(path: String, argsWithIndex: List[(ScalaParameter, Int)]): String =
    path
      .split("((?=/)|(?=\\.))")
      .toList
      .map {
        case segment if(!segment.startsWith("/:")) => segment
        case segment =>
          argsWithIndex
            .find(_._1.originalName == segment.drop(2))
            .fold(segment) { case (arg, index) => s"""/$${${extractor(arg, index)}}""" }
      }
      .mkString

  def query(argsWithIndex: List[(ScalaParameter, Int)]): String = {
    val queryArguments = argsWithIndex.map { case (arg, index) =>
      val interpolator = queryInterpolator(arg)
      val extractorAndArgument = extractor(arg, index)

      s"""${interpolator}"${arg.originalName}=$${${extractorAndArgument}}""""
    }

    queryArguments match {
      case Nil => ""
      case args => args.mkString(" ? ", " & ", "")
    }
  }

  def syntax: String = {
    val args = (operation.pathParameters ++ operation.queryParameters)
      .zipWithIndex
      .map { case (arg, index) => (arg, index + 1) }

    val p = path(operation.path, args.filter(_._1.location == ParameterLocation.Path))
    val q = query(args.filter(_._1.location == ParameterLocation.Query))

    val method = operation.method
    val handlerName = operation.name
    val handlerArguments = args.map((getter _).tupled).mkString(", ")

    s"""case ${method}(p"${p}"${q}) => ${handlerName}(${handlerArguments})"""
  }
}

case class SimpleRouterTrait(resource: ScalaResource, handlers: List[RouterHandler], patternMatches: List[RouterPatternMatch]) {
  def syntax = s"""
    trait ${resource.plural}Routes extends _root_.play.api.routing.SimpleRouter {

      ${handlers.map(_.syntax).mkString("\n")}

      override def routes: _root_.play.api.routing.Router.Routes = {
        ${patternMatches.map(_.syntax).mkString("\n")}
      }
    }
  """
}

case class SirdRoutersPackage(packageObject: String, routers: List[SimpleRouterTrait]) {
  def syntax = s"""
    package ${packageObject.split('.').dropRight(1).mkString(".")}

    package object ${packageObject.split('.').last} {
      import _root_.play.api.routing.sird._

      ${routers.map(_.syntax).mkString("\n")}
    }
  """
}

object SirdRouters extends Component {

  def sirdRoutersPackage(service: ScalaService) = {
    val routers = service.resources
      .map { resource =>
        val handlers = resource.operations.map(RouterHandler).toList
        val patternMatches = resource.operations.map(RouterPatternMatch(_, service.namespaces.bindables)).toList
        SimpleRouterTrait(resource, handlers, patternMatches)
      }
      .toList

    SirdRoutersPackage(service.namespaces.routers, routers)
  }

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val syntax = sirdRoutersPackage(service).syntax
    Validated.validNel(syntax)
  }
}
