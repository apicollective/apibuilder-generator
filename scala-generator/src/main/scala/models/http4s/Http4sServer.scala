package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm

import scala.generator.{ScalaClientMethodConfigs, ScalaDatatype, ScalaOperation, ScalaPrimitive, ScalaResource, ScalaUtil}
import lib.Text._
import lib.VersionTag

import scala.models.JsonImports

object Http4sServer {
  def pathExtractorName(sp: ScalaPrimitive, min: Option[Long], max: Option[Long]): String = {
    val minPart = min.fold("")(v => s"$v")
    val maxPart = max.fold("")(v => s"To$v")
    s"${sp.shortName}${minPart}${maxPart}Val"
  }

  def queryExtractorName(name: String, sdt: ScalaDatatype, min: Option[Long], max: Option[Long], default: Option[String], collType: Option[String] = None): (String, String) = {
    sdt match {
      case sp: ScalaPrimitive =>
        val collPart = collType.getOrElse("")
        val minPart = min.fold("")(v => s"$v")
        val maxPart = max.fold("")(v => s"To$v")
        val defPart = default.fold("")(v => s"Def$v")
        (s"${name.capitalize}$collPart${sp.shortName}$minPart$maxPart${defPart}Matcher", name)
      case ScalaDatatype.List(nested) =>
        val (extractor, handler) = queryExtractorName(name, nested, min, max, default, Some("List"))
        (extractor, s"cats.data.Validated.Valid($handler)")
      case ScalaDatatype.Option(nested) =>
        queryExtractorName(name, nested, min, max, default, Some("Opt"))
    }
  }
}

case class Http4sServer(form: InvocationForm,
                        ssd: ScalaService,
                        config: ScalaClientMethodConfigs.Http4s) {

  val generatorUtil = new ScalaGeneratorUtil(config)

  val sortedResources = ssd.resources.sortWith { _.plural.toLowerCase < _.plural.toLowerCase }

  val version: Option[Int] = VersionTag(form.service.version).major

  def generate(): String = {
    val versionCapture = version.fold("") { ver =>
      s"""
         |object ApiVersion {
         |  val ApiVersionMajor = {
         |    import org.http4s.syntax.string._
         |    "X-Apidoc-Version-Major".ci
         |  }
         |
         |  def apply(req: org.http4s.Message): Boolean = req.headers.get(ApiVersionMajor) match {
         |    case Some(v) if v.value == "$ver" => true
         |    case _ => false
         |  }
         |}
         |""".stripMargin
    }

    val resourcesAndRoutes = sortedResources.toList.map { res =>
      res -> res.operations.map(op => Route(res, op, config))
    }

    val allRoutes = resourcesAndRoutes.map(_._2).flatten

    val path = genPathExtractors(allRoutes)
    val query = genQueryExtractors(allRoutes)

    val resources = resourcesAndRoutes.map { case (resource, routes) =>

      val name = lib.Text.snakeToCamelCase(lib.Text.camelCaseToUnderscore(ScalaUtil.toClassName(resource.resource.`type`)).toLowerCase + "_routes").capitalize

      s"""trait $name {
         |  implicit def circeJsonDecoder[A](implicit decoder: io.circe.Decoder[A]) = org.http4s.circe.jsonOf[A]
         |  implicit def circeJsonEncoder[A](implicit encoder: io.circe.Encoder[A]) = org.http4s.circe.jsonEncoderOf[A]
         |
 |       |${routes.map(_.operation().mkString("\n")).mkString("\n\n").indent(2)}
         |
         |  def service() = org.http4s.HttpService {
         |${routes.map(_.route(version).mkString("\n")).mkString("\n\n").indent(4)}
         |  }
         |}
         |""".stripMargin
    }.mkString("\n")

    s"""package ${ssd.namespaces.base}.server
       |
       |import org.http4s.dsl._
       |${JsonImports(form.service).mkString("\n")}
       |$versionCapture
       |${path.mkString("\n")}
       |${query.mkString("\n")}
       |$resources
     """.stripMargin
  }

  def genPathExtractors(routes: List[Route]): Seq[String] = {
    (routes.flatMap(_.pathSegments).collect {
      case Extracted(_, dt, min, max) => (dt, min, max)
    }).distinct.map { case (dt, min, max) =>

      val filter = (dt, min, max) match {
        case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), None) => s".filter(_ >= $min)"
        case (ScalaPrimitive.Integer | ScalaPrimitive.Long, None, Some(max)) => s".filter(_ <= $max)"
        case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), Some(max)) => s".filter(v => v >= $min && v <= $max)"
        case (ScalaPrimitive.String, Some(min), None) => s".filter(_.length >= $min)"
        case (ScalaPrimitive.String, None, Some(max)) => s".filter(_.length <= $max)"
        case (ScalaPrimitive.String, Some(min), Some(max)) => s".filter(s => s.length >= $min && s.length <= $max)"
        case _ => ""
      }

      val toOption = dt match {
        case ScalaPrimitive.String => s"Some(s)$filter"
        case ScalaPrimitive.Boolean => "scala.util.Try(s.toBoolean).toOption"
        case ScalaPrimitive.Integer => s"scala.util.Try(s.toInt).toOption$filter"
        case ScalaPrimitive.Long => s"scala.util.Try(s.toLong).toOption$filter"
        case ScalaPrimitive.Double => "scala.util.Try(s.toDouble).toOption"
        case ScalaPrimitive.Decimal => "scala.util.Try(BigDecimal(s)).toOption"
        case ScalaPrimitive.Uuid => "scala.util.Try(java.util.UUID.fromString(s)).toOption"
        case ScalaPrimitive.DateIso8601Java => "scala.util.Try(java.time.LocalDate.parse(s)).toOption"
        case ScalaPrimitive.DateTimeIso8601Java => "scala.util.Try(java.time.Instant.parse(s)).toOption"
        case ScalaPrimitive.Enum(_, _) => s"${dt.fullName}.fromString(s)"
        case _ => s"""throw new UnsupportedOperationException("Type ${dt.fullName} is not supported as a capture value")"""
      }

      Seq(
        s"object ${Http4sServer.pathExtractorName(dt, min, max)} {",
        s"  def unapply(s: String): Option[${dt.fullName}] = $toOption",
        "}\n"
      ).mkString("\n")
    }
  }

  def genQueryExtractors(routes: List[Route]): Seq[String] = {
    val d = routes.flatMap(_.op.queryParameters).map { st =>
      (st.name, st.datatype, st.param.minimum, st.param.maximum, st.default)
    }
    d.distinct.map { case (name: String, dt: ScalaDatatype, min: Option[Long], max: Option[Long], default: Option[String]) =>
      val extractorName = Http4sServer.queryExtractorName(name, dt, min, max, default)._1

      dt match {
        case ScalaDatatype.Option(ScalaDatatype.List(sp: ScalaPrimitive)) =>
          s"""object $extractorName extends OptionalMultiQueryParamDecoderMatcher[${sp.shortName}]("$name")"""
        case ScalaDatatype.List(sp: ScalaPrimitive) =>
          s"""object $extractorName extends OptionalMultiQueryParamDecoderMatcher[${sp.shortName}]("$name")"""
        case ScalaDatatype.Option(sp: ScalaPrimitive) =>
          s"""object $extractorName extends OptionalQueryParamDecoderMatcher[${sp.shortName}]("$name")"""
        case sp: ScalaPrimitive =>
          val defPart = default.map { d =>
            val default = sp match {
              case ScalaPrimitive.Long if d.toLowerCase.lastOption != Some('l') => s"${d}L"
              case _ => d
            }
            s".orElse(Some($default))"
          }

          val filterPart = (sp, min, max) match {
            case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), None) => Some(s".filter(_ >= $min)")
            case (ScalaPrimitive.Integer | ScalaPrimitive.Long, None, Some(max)) => Some(s".filter(_ <= $max)")
            case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), Some(max)) => Some(s".filter(v => v >= $min && v <= $max)")
            case (ScalaPrimitive.String, Some(min), None) => Some(s".filter(_.length >= $min)")
            case (ScalaPrimitive.String, None, Some(max)) => Some(s".filter(_.length <= $max)")
            case (ScalaPrimitive.String, Some(min), Some(max)) => Some(s".filter(s => s.length >= $min && s.length <= $max)")
            case _ => None
          }


          if (defPart.isDefined || filterPart.isDefined) {
            Seq(
              s"""object $extractorName extends QueryParamDecoderMatcher[${sp.shortName}]("$name") {""",
              s"""  override def unapply(params: Map[String, Seq[String]]) = super.unapply(params)${defPart.getOrElse("")}${filterPart.getOrElse("")}""",
              s"""}"""
            ).mkString("\n")
          } else {
            s"""object $extractorName extends QueryParamDecoderMatcher[${sp.shortName}]("$name")"""
          }
      }
    }.map(_ + "\n")
  }
}

sealed trait PathSegment
case class Literal(name: String) extends PathSegment
case class PlainString(name: String) extends PathSegment
case class Extracted(name: String, dt: ScalaPrimitive, min: Option[Long], max: Option[Long]) extends PathSegment

case class Route(resource: ScalaResource, op: ScalaOperation, config: ScalaClientMethodConfigs.Http4s) {
  val pathSegments = op.path.split("/").filterNot(_.isEmpty).map { segment =>
    if(!segment.startsWith(":")) {
      Literal(segment)
    } else {
      val truncated = segment.drop(1)
      op.pathParameters.find(_.name == truncated).fold(Literal(segment): PathSegment) { scalaParameter =>
        scalaParameter.datatype match {
          case ScalaPrimitive.String if scalaParameter.param.minimum.isEmpty && scalaParameter.param.maximum.isEmpty => PlainString(truncated)
          case dt: ScalaPrimitive => Extracted(truncated, dt, scalaParameter.param.minimum, scalaParameter.param.maximum)
        }
      }
    }
  }

  def operation(): Seq[String] = {
    def params =
      Seq(Some("_req: org.http4s.Request")) ++
      op.nonHeaderParameters.map { field =>
        val typ = field.datatype match {
          case ScalaDatatype.Option(nested) => nested.name
          case _ => field.datatype.name
        }
        Some(s"${ScalaUtil.quoteNameIfKeyword(field.name)}: $typ")
      } ++
      Seq(op.body.map(body => s"body: => org.http4s.DecodeResult[${body.datatype.name}]"))

    Seq(
      s"def ${op.name}(",
      params.flatten.mkString(",\n").indent(2),
      s"): ${config.asyncType}[org.http4s.Response]"
    )
  }

  def route(version: Option[Int]): Seq[String] = {
    val args = (
      Seq(Some("_req")) ++
      op.nonHeaderParameters.map(field => Some(s"${ScalaUtil.quoteNameIfKeyword(field.name)}")) ++
      Seq(op.body.map(body => s"_req.attemptAs[${body.datatype.name}]"))
    ).flatten.mkString(", ")

    val path = (
      Seq("Root") ++
      pathSegments.collect {
        case Literal(n) => s""""$n""""
        case PlainString(n) => s"$n"
        case Extracted(name, dt, min, max) => s"${Http4sServer.pathExtractorName(dt, min, max)}($name)"
      }
    ).mkString(" / ")

    val query = (
      op.queryParameters.map { st =>
        val (extractor, handler) = Http4sServer.queryExtractorName(st.name, st.datatype, st.param.minimum, st.param.maximum, st.default)
        s"$extractor($handler)"
      }
    ).mkString(" +& ")

    val queryStart = if (query.size > 0) """ :? """ else ""

    val verFilter = version.fold("")(_ => " if ApiVersion(_req)")

    Seq(
      s"case _req @ ${op.method} -> $path$queryStart$query$verFilter =>",
      s"  ${op.name}($args)"
    )
  }
}
