package scala.models.http4s.server

import io.apibuilder.generator.v0.models.InvocationForm

import scala.generator.{ScalaClientMethodConfigs, ScalaDatatype, ScalaPrimitive, ScalaUtil}
import scala.models.JsonImports
import scala.models.http4s.{ScalaGeneratorUtil, ScalaService}
import lib.Text._
import lib.VersionTag

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
         |${routes.map(_.operation().mkString("\n")).mkString("\n\n").indent(2)}
         |
         |  def apiVersionMatch(req: org.http4s.Message): Boolean = ApiVersion(req)
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
    val distinctParams = routes.flatMap(_.op.queryParameters).groupBy { param =>
      val typ = param.datatype match {
        case ScalaDatatype.Option(container: ScalaDatatype.Container) => container
        case other => other
      }
      (param.originalName, typ, param.param.minimum, param.param.maximum, param.default)
    }.values.toList.map(_.head)
    distinctParams.map { param =>
      val name = param.originalName
      val extractorName = Http4sServer.queryExtractorName(name, param.datatype, param.param.minimum, param.param.maximum, param.default)._1

      param.datatype match {
        case ScalaDatatype.Option(ScalaDatatype.List(sp: ScalaPrimitive)) =>
          s"""object $extractorName extends OptionalMultiQueryParamDecoderMatcher[${sp.shortName}]("$name")"""
        case ScalaDatatype.List(sp: ScalaPrimitive) =>
          s"""object $extractorName extends OptionalMultiQueryParamDecoderMatcher[${sp.shortName}]("$name")"""
        case ScalaDatatype.Option(sp: ScalaPrimitive) =>
          s"""object $extractorName extends OptionalQueryParamDecoderMatcher[${sp.shortName}]("$name")"""
        case sp: ScalaPrimitive =>
          val defPart = param.default.map { d =>
            val default = sp match {
              case ScalaPrimitive.Long if d.toLowerCase.lastOption != Some('l') => s"${d}L"
              case _ => d
            }
            s".orElse(Some($default))"
          }

          val filterPart = (sp, param.param.minimum, param.param.maximum) match {
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

