package scala.models

import lib.generator.{CodeGenerator, GeneratorUtil}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import scala.generator.{ScalaDatatype, ScalaPrimitive, ScalaOperation, ScalaParameter, ScalaResource, ScalaService, ScalaUtil}

object Play2RouteGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    new Play2RouteGenerator(form).invoke
  }
}


/**
 * Generates a Play routes file based on the service description
 * from api.json
 */
case class Play2RouteGenerator(form: InvocationForm) {

  private[this] val GlobalPad = 5

  private[this] val service = form.service
  private[this] val scalaService = ScalaService(service, Config(form.attributes, Config.PlayDefaultConfig))

  def invoke(): Either[Seq[String], Seq[File]] = {
    scalaService.resources.flatMap { resource =>
      resource.operations.map { op =>
        Play2Route(scalaService, op, resource)
      }
    } match {
      case Nil => {
        Left(
          Seq(s"Service[${service.organization.key}/${service.application.key}] does not have any resource operations - cannot generate play routes")
        )
      }
      case all => {
        val maxVerbLength = all.map(_.verb.toString.length).toSeq.sorted.last
        val maxUrlLength = all.map(_.url.length).toSeq.sorted.last
        val (paramStart, pathStart) = all.partition(_.url.startsWith("/:"))

        Right(
          Seq(File(
            "routes",
            None,
            ApidocComments(form.service.version, form.userAgent).forPlayRoutes + "\n\n" +
              (pathStart ++ paramStart).map { r =>
                Seq(
                  r.verb,
                  " " * (maxVerbLength - r.verb.toString.length + GlobalPad),
                  r.url,
                  " " * (maxUrlLength - r.url.length + GlobalPad),
                  r.method,
                  "(",
                  r.params.mkString(", "),
                  ")"
                ).mkString("")
              }.mkString("\n")
          ))
        )
      }
    }
  }
}

private[models] case class Play2Route(
  ssd: ScalaService,
  op: ScalaOperation,
  resource: ScalaResource
) {

  val verb = op.method
  val url = op.path
  val params = parametersWithTypesAndDefaults(
    op.parameters.
      filter { param =>
        param.location match {
          case ParameterLocation.Form => false
          case ParameterLocation.Query => true
          case ParameterLocation.Path => true
          case ParameterLocation.Header => false
          case ParameterLocation.UNDEFINED(_) => false
        }
      }.
      filter { param =>
        param.datatype match {
          case (_: ScalaPrimitive) | ScalaDatatype.List(_) => true
          case ScalaDatatype.Map(_) => false
          case ScalaDatatype.Option(inner) => true
        }
      }
  )

  val method = "%s.%s".format(
    "controllers." + lib.Text.underscoreAndDashToInitCap(resource.plural),
    GeneratorUtil.urlToMethodName(resource.path, resource.operations.map(_.path), op.method, url)
  )

  private def parametersWithTypesAndDefaults(params: Iterable[ScalaParameter]): Iterable[String] = {
    params.map { param =>
      Seq(
        Some(definition(param)),
        param.default.flatMap( d =>
          param.datatype match {
            case ScalaDatatype.List(v) => Some("?= Nil")
            case ScalaDatatype.Map(_) => Some("?= Map()")
            case ScalaDatatype.Option(inner) => None
            case p: ScalaPrimitive => Some("?= " + defaultForPrimitive(p, d))
          }
        )
      ).flatten.mkString(" ")
    }
  }

  private def defaultForPrimitive(
    primitive: ScalaPrimitive,
    value: String
  ): String = primitive match {
    case ScalaPrimitive.String | ScalaPrimitive.DateIso8601Joda | ScalaPrimitive.DateIso8601Java | ScalaPrimitive.DateTimeIso8601Joda | ScalaPrimitive.DateTimeIso8601JavaInstant | ScalaPrimitive.DateTimeIso8601JavaOffsetDateTime | ScalaPrimitive.Uuid | ScalaPrimitive.Enum(_, _) => {
      value
    }
    case ScalaPrimitive.Integer | ScalaPrimitive.Double | ScalaPrimitive.Long | ScalaPrimitive.Boolean | ScalaPrimitive.Decimal => {
      value
    }
    case ScalaPrimitive.ObjectAsPlay => {
      "play.api.libs.json.Json.parse(%s)".format(ScalaUtil.wrapInQuotes(value))
    }
    case ScalaPrimitive.ObjectAsCirce => {
      "play.api.libs.json.Json.parse(%s)".format(ScalaUtil.wrapInQuotes(value))
    }
    case ScalaPrimitive.JsonValueAsPlay => {
      "play.api.libs.json.Json.parse(%s)".format(ScalaUtil.wrapInQuotes(value))
    }
    case ScalaPrimitive.JsonValueAsCirce => {
      "play.api.libs.json.Json.parse(%s)".format(ScalaUtil.wrapInQuotes(value))
    }
    case ScalaPrimitive.Model(_, _) | ScalaPrimitive.Union(_, _) | ScalaPrimitive.Unit => {
      sys.error(s"Unsupported type[$primitive] for default values")
    }
  }

  private def definition(scalaParam: ScalaParameter): String = {
    val datatypeName = scalaParam.datatype match {
      case ScalaDatatype.Map(_) => {
        sys.error("Cannot have maps as parameters")
      }
      case ScalaDatatype.List(inner) => {
        s"List[${inner.name}]"
      }
      case ScalaDatatype.Option(ScalaDatatype.List(inner)) => {
        s"_root_.scala.Option[List[${inner.name}]]"
      }
      case datatype @ (ScalaDatatype.Option(_) | _: ScalaPrimitive) =>
        datatype.name
    }

    // Quote special chars (e.g. `foo[bar]`) but not reserved words (eg type)
    if (ScalaUtil.needsQuoting(scalaParam.originalName)) {
      s"${ScalaUtil.quoteNameIfKeyword(scalaParam.originalName)}: $datatypeName"
    } else {
      s"${scalaParam.originalName}: $datatypeName"
    }
  }
}

