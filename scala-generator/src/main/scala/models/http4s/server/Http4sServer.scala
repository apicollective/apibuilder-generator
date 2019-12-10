package scala.models.http4s.server

import io.apibuilder.generator.v0.models.InvocationForm

import scala.generator.{ScalaClientMethodConfigs, ScalaDatatype, ScalaParameter, ScalaPrimitive, ScalaUtil}
import scala.models.JsonImports
import scala.models.http4s.{ScalaGeneratorUtil, ScalaService}
import lib.Text
import lib.Text._
import lib.VersionTag

import scala.generator.ScalaPrimitive.{Uuid, Decimal}

case class PathExtractor(name: String, filter: Option[String])
case class QueryExtractor(name: String, handler: String)

object Http4sServer {
  def typeName(ssd: ScalaService, dataType: ScalaDatatype): String = dataType match {
    case model: ScalaPrimitive.Model if model.namespaces.base == ssd.service.namespace => model.shortName
    case model: ScalaPrimitive.Model => ScalaUtil.toClassName(model.name)
    case enum: ScalaPrimitive.Enum if enum.namespaces.base == ssd.service.namespace => enum.shortName
    case enum: ScalaPrimitive.Enum => ScalaUtil.toClassName(enum.name)
    case sp: ScalaPrimitive => sp.shortName
    case other => other.name
  }

  def pathExtractor(ssd: ScalaService, parameter: ScalaParameter): PathExtractor = {
    val filter = (parameter.datatype, parameter.param.minimum, parameter.param.maximum) match {
      case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), None) => Some(s".filter(_ >= $min)")
      case (ScalaPrimitive.Integer | ScalaPrimitive.Long, None, Some(max)) => Some(s".filter(_ <= $max)")
      case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), Some(max)) => Some(s".filter(v => v >= $min && v <= $max)")
      case (ScalaPrimitive.String, Some(min), None) => Some(s".filter(_.length >= $min)")
      case (ScalaPrimitive.String, None, Some(max)) => Some(s".filter(_.length <= $max)")
      case (ScalaPrimitive.String, Some(min), Some(max)) => Some(s".filter(s => s.length >= $min && s.length <= $max)")
      case _ => None
    }

    val minMaxPart = filter.fold("")(_ => parameter.param.minimum.fold("")(v => s"$v") + parameter.param.maximum.fold("")(v => s"To$v"))

    PathExtractor(s"${typeName(ssd, parameter.datatype)}${minMaxPart}Val", filter)
  }

  def queryExtractor(ssd: ScalaService, param: ScalaParameter): QueryExtractor = {

    def recurse(typ: ScalaDatatype, collPart: String, withMinMaxDef: Boolean): QueryExtractor = {
      typ match {
        case sp: ScalaPrimitive if withMinMaxDef =>
          val minPart = param.param.minimum.fold("")(v => s"$v")
          val maxPart = param.param.maximum.fold("")(v => s"To$v")
          val defPart = param.param.default.fold("")(v => s"Def${ScalaUtil.toClassName(v)}")
          QueryExtractor(s"${param.name.capitalize}$collPart${typeName(ssd, sp)}$minPart$maxPart${defPart}Matcher", param.asScalaVal)
        case sp: ScalaPrimitive =>
          QueryExtractor(s"${param.name.capitalize}$collPart${typeName(ssd, sp)}Matcher", param.asScalaVal)
        case ScalaDatatype.List(nested) =>
          val extractor = recurse(nested, "List", false)
          QueryExtractor(extractor.name, s"cats.data.Validated.Valid(${extractor.handler})")
        case ScalaDatatype.Option(nested) =>
          recurse(nested, "Opt", true)
      }
    }
    recurse(param.datatype, "", true)
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
         |    "X-Apidoc-Version-Major".ci
         |  }
         |
         |  def apply(req: ${config.messageClass}): Boolean = req.headers.get(ApiVersionMajor) match {
         |    case Some(v) if v.value == "$ver" => true
         |    case _ => false
         |  }
         |}
         |""".stripMargin
    }

    val resourcesAndRoutes = sortedResources.toList.map { res =>
      res -> res.operations.map(op => Route(ssd, res, op, config))
    }

    val allRoutes = resourcesAndRoutes.map(_._2).flatten

    val path = genPathExtractors(allRoutes)
    val query = genQueryExtractors(allRoutes)

    val resources = resourcesAndRoutes.map { case (resource, routes) =>

      val name = lib.Text.snakeToCamelCase(lib.Text.camelCaseToUnderscore(ScalaUtil.toClassName(resource.resource.`type`)).toLowerCase + "_routes").capitalize

      s"""${config.routeKind} $name${config.asyncTypeParam().map(p => s"[$p]").getOrElse("")}${config.routeExtends.getOrElse("")} {
         |${config.matchersImport}
         |  implicit def circeJsonDecoder[A](implicit decoder: _root_.io.circe.Decoder[A]${config.asyncTypeParam().map(p => s", sync: Sync[F]").getOrElse("")}) = ${config.generateCirceJsonOf("A")}
         |  implicit def circeJsonEncoder[A](implicit encoder: _root_.io.circe.Encoder[A]${config.asyncTypeParam().map(p => s", sync: Sync[F]").getOrElse("")}) = ${config.generateCirceJsonEncoderOf("A")}
         |
         |${routes.map(_.operation().mkString("\n")).mkString("\n\n").indent(2)}
         |
         |  def apiVersionMatch(req: ${config.messageClass}): Boolean = ApiVersion(req)
         |
         |  def service()${config.asyncTypeParam().map(p => s"(implicit sync: Sync[F])").getOrElse("")} = ${config.httpServiceClass} {
         |${routes.map(_.route(version).mkString("\n")).mkString("\n\n").indent(4)}
         |  }
         |}
         |""".stripMargin
    }.mkString("\n")

    val enumDecoders = allRoutes.flatMap(_.op.queryParameters).map(_.datatype).collect {
      case enum: ScalaPrimitive.Enum => enum
      case ScalaDatatype.Option(enum: ScalaPrimitive.Enum) => enum
      case ScalaDatatype.List(enum: ScalaPrimitive.Enum) => enum
      case ScalaDatatype.Option(ScalaDatatype.List(enum: ScalaPrimitive.Enum)) => enum
    }.distinct.map { enum =>
      s"""
         |implicit lazy val ${Text.initLowerCase(Http4sServer.typeName(ssd, enum))}QueryParamDecoder: org.http4s.QueryParamDecoder[${enum.fullName}] =
         |  org.http4s.QueryParamDecoder.fromUnsafeCast[${enum.fullName}](p => ${enum.fullName}.fromString(p.value).get)("${enum.fullName}")""".stripMargin
    }.mkString("\n")

    s"""package ${ssd.namespaces.base}.server
       |${config.serverImports}
       |${JsonImports(form.service).mkString("\n")}
       |
       |private[server] ${config.matcherKind} Matchers${config.asyncTypeParam().map(p => s"[$p]").getOrElse("")}${config.matchersExtends.getOrElse("")} {
       |
       |  implicit lazy val queryParamDecode${Decimal.shortName}: org.http4s.QueryParamDecoder[${Decimal.fullName}] =
       |    org.http4s.QueryParamDecoder.fromUnsafeCast[${Decimal.fullName}](p => ${Decimal.fromStringValue("p.value")})("${Decimal.fullName}")
       |
       |  implicit lazy val queryParamDecode${ssd.attributes.dateTimeType.dataType.shortName}: org.http4s.QueryParamDecoder[${ssd.attributes.dateTimeType.dataType.fullName}] =
       |    org.http4s.QueryParamDecoder.fromUnsafeCast[${ssd.attributes.dateTimeType.dataType.fullName}](p => ${ssd.attributes.dateTimeType.dataType.fromStringValue("p.value")})("${ssd.attributes.dateTimeType.dataType.fullName}")
       |
       |  implicit lazy val queryParamDecode${ssd.attributes.dateType.dataType.shortName}: org.http4s.QueryParamDecoder[${ssd.attributes.dateType.dataType.fullName}] =
       |    org.http4s.QueryParamDecoder.fromUnsafeCast[${ssd.attributes.dateType.dataType.fullName}](p => ${ssd.attributes.dateType.dataType.fromStringValue("p.value")})("${ssd.attributes.dateType.dataType.fullName}")
       |
       |  implicit lazy val queryParamDecode${Uuid.shortName}: org.http4s.QueryParamDecoder[${Uuid.fullName}] =
       |    org.http4s.QueryParamDecoder.fromUnsafeCast[${Uuid.fullName}](p => ${Uuid.fromStringValue("p.value")})("${Uuid.fullName}")
       |${enumDecoders.indent(2)}
       |${versionCapture.indent(2)}
       |${path.mkString("\n").indent(2)}
       |${query.mkString("\n").indent(2)}
       |}
       |
       |$resources
     """.stripMargin
  }

  def genPathExtractors(routes: List[Route]): Seq[String] = {
    val distinctParams: Seq[(PathExtractor, ScalaParameter)] = routes.flatMap(_.pathSegments).collect {
      case Extracted(param) => Http4sServer.pathExtractor(ssd, param) -> param
    }.toMap.toList.sortBy(_._1.name)

    distinctParams.map { case (extractor, param) =>
      val filter = extractor.filter.getOrElse("")

      val toOption = param.datatype match {
        case sp: ScalaPrimitive =>
          sp match {
            case dt @ ScalaPrimitive.String => s"Some(${dt.fromStringValue("s")})$filter"
            case dt @ (ScalaPrimitive.Integer | ScalaPrimitive.Long) => s"scala.util.Try(${dt.fromStringValue("s")}).toOption$filter"
            case dt @ (ScalaPrimitive.Boolean | ScalaPrimitive.Double | ScalaPrimitive.Decimal | ScalaPrimitive.Uuid | _: ScalaPrimitive.DateIso8601 | _: ScalaPrimitive.DateTimeIso8601) => s"scala.util.Try(${dt.fromStringValue("s")}).toOption"
            case enum: ScalaPrimitive.Enum => s"${enum.name}.fromString(s)"
            case _ => s"""None // Type ${param.datatype.name} is not supported as a capture value"""
          }
        case _ => s"""None // Type ${param.datatype.name} is not supported as a capture value"""
      }

      s"""
         |object ${extractor.name} {
         |  def unapply(s: String): Option[${param.datatype.name}] = $toOption
         |}""".stripMargin
    }
  }

  def genQueryExtractors(routes: List[Route]): Seq[String] = {
    val distinctParams = routes.flatMap(_.op.queryParameters).map { param =>
      Http4sServer.queryExtractor(ssd, param) -> param
    }.toMap.toList.sortBy(_._1.name)
    distinctParams.map { case (QueryExtractor(extractorName, _), param) =>
      param.datatype match {
        case ScalaDatatype.Option(ScalaDatatype.List(sp: ScalaPrimitive)) =>
          s"""object $extractorName extends OptionalMultiQueryParamDecoderMatcher[${sp.name}]("${param.originalName}")"""
        case ScalaDatatype.List(sp: ScalaPrimitive) =>
          s"""object $extractorName extends OptionalMultiQueryParamDecoderMatcher[${sp.name}]("${param.originalName}")"""
        case ScalaDatatype.Option(sp: ScalaPrimitive) =>
          val filterPart = (sp, param.param.minimum, param.param.maximum) match {
            case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), None) => Some(s".filter(_.forall(_ >= $min))")
            case (ScalaPrimitive.Integer | ScalaPrimitive.Long, None, Some(max)) => Some(s".filter(_.forall(_ <= $max))")
            case (ScalaPrimitive.Integer | ScalaPrimitive.Long, Some(min), Some(max)) => Some(s".filter(_.forall(v => v >= $min && v <= $max))")
            case (ScalaPrimitive.String, Some(min), None) => Some(s".filter(_.forall(_.length >= $min))")
            case (ScalaPrimitive.String, None, Some(max)) => Some(s".filter(_.forall(_.length <= $max))")
            case (ScalaPrimitive.String, Some(min), Some(max)) => Some(s".filter(_.forall(s => s.length >= $min && s.length <= $max))")
            case _ => None
          }
          filterPart.fold(
            s"""object $extractorName extends OptionalQueryParamDecoderMatcher[${sp.name}]("${param.originalName}")"""
          ){ filterPart =>
            s"""object $extractorName extends OptionalQueryParamDecoderMatcher[${sp.name}]("${param.originalName}") {
               |  override def unapply(params: Map[String, Seq[String]]) = super.unapply(params)$filterPart
               |}""".stripMargin
          }
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

            s"""object $extractorName extends QueryParamDecoderMatcher[${sp.name}]("${param.originalName}") {
               |  override def unapply(params: Map[String, Seq[String]]) = super.unapply(params)${defPart.getOrElse("")}${filterPart.getOrElse("")}
               |}""".stripMargin
          } else {
            s"""object $extractorName extends QueryParamDecoderMatcher[${sp.name}]("${param.originalName}")"""
          }
      }
    }.map("\n" + _)
  }
}

