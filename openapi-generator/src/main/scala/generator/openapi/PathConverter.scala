package generator.openapi

import io.apibuilder.spec.v0.{models => ab}
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap
import scala.collection.mutable

object PathConverter {

  private val PathVariablePattern = """\{(\w+)\}""".r

  def convert(resources: Seq[ab.Resource], resolver: TypeResolver): Paths = {
    val allOps = resources.flatMap { resource =>
      resource.operations.map { op =>
        val oasPath = toOpenApiPath(op.path, resource.path)
        (oasPath, op)
      }
    }

    val normalizedOps = normalizePathVariables(allOps)

    val grouped = normalizedOps.groupBy(_._1).toSeq.sortBy(_._1)

    val pathItems = grouped.map { case (path, ops) =>
      val pathItem = ops.foldLeft(PathItem()) { case (pi, (_, op)) =>
        val operation = convertOperation(op, resolver)
        op.method match {
          case ab.Method.Get => pi.copy(get = Some(operation))
          case ab.Method.Put => pi.copy(put = Some(operation))
          case ab.Method.Post => pi.copy(post = Some(operation))
          case ab.Method.Delete => pi.copy(delete = Some(operation))
          case ab.Method.Patch => pi.copy(patch = Some(operation))
          case ab.Method.Head => pi.copy(head = Some(operation))
          case ab.Method.Options => pi.copy(options = Some(operation))
          case ab.Method.Trace => pi.copy(trace = Some(operation))
          case _ => pi
        }
      }
      path -> pathItem
    }

    Paths(ListMap.from(pathItems))
  }

  private def convertOperation(op: ab.Operation, resolver: TypeResolver): Operation = {
    val (formParams, regularParams) = op.parameters.partition(_.location == ab.ParameterLocation.Form)

    val oasParams = regularParams.map(convertParameter(_, resolver))

    val bodyFromOp = op.body.flatMap(convertBody(_, resolver))
    val formBody = if (formParams.nonEmpty) Some(convertFormParams(formParams, resolver)) else None

    val requestBody = bodyFromOp.orElse(formBody)

    val responses = convertResponses(op.responses, resolver)

    Operation(
      operationId = None,
      summary = None,
      description = op.description,
      deprecated = op.deprecation.map(_ => true),
      parameters = oasParams.map(Right(_)).toList,
      requestBody = requestBody.map(Right(_)),
      responses = responses,
    )
  }

  private def convertParameter(p: ab.Parameter, resolver: TypeResolver): Parameter = {
    val in = p.location match {
      case ab.ParameterLocation.Query => ParameterIn.Query
      case ab.ParameterLocation.Header => ParameterIn.Header
      case ab.ParameterLocation.Path => ParameterIn.Path
      case ab.ParameterLocation.Form => ParameterIn.Query
      case _ => ParameterIn.Query
    }
    Parameter(
      name = p.name,
      in = in,
      description = p.description,
      required = Some(p.required),
      deprecated = p.deprecation.map(_ => true),
      schema = resolver.toSchema(p.`type`),
      example = p.example.map(sttp.apispec.ExampleSingleValue(_)),
    )
  }

  private def convertBody(body: ab.Body, resolver: TypeResolver): Option[RequestBody] = {
    resolver.resolve(body.`type`) match {
      case ResolvedType.NoSchema => None
      case _ =>
        val schema = resolver.toSchemaOrEmpty(body.`type`)
        Some(
          RequestBody(
            description = body.description,
            content = ListMap("application/json" -> MediaType(schema = Some(schema))),
            required = Some(true),
          ),
        )
    }
  }

  private def convertFormParams(params: Seq[ab.Parameter], resolver: TypeResolver): RequestBody = {
    val properties = ListMap.from(params.map { p =>
      p.name -> resolver.toSchemaOrEmpty(p.`type`)
    })
    val required = params.filter(_.required).map(_.name).toList
    val schema = sttp.apispec.Schema(
      `type` = Some(List(sttp.apispec.SchemaType.Object)),
      properties = properties,
      required = required,
    )
    RequestBody(
      content = ListMap("application/x-www-form-urlencoded" -> MediaType(schema = Some(schema))),
      required = if (required.nonEmpty) Some(true) else None,
    )
  }

  private def convertResponses(responses: Seq[ab.Response], resolver: TypeResolver): Responses = {
    val entries = responses.map { r =>
      val key: ResponsesKey = r.code match {
        case ab.ResponseCodeInt(n) => ResponsesCodeKey(n)
        case ab.ResponseCodeOption.Default => ResponsesDefaultKey
        case ab.ResponseCodeOption.UNDEFINED(s) =>
          scala.util.Try(s.toInt).map(ResponsesCodeKey(_)).getOrElse(ResponsesDefaultKey)
        case ab.ResponseCodeUndefinedType(s) =>
          scala.util.Try(s.toInt).map(ResponsesCodeKey(_)).getOrElse(ResponsesDefaultKey)
      }
      val content = resolver.responseContent(r.`type`)
      val description = r.description.getOrElse(defaultDescription(key))
      val response = Response(
        description = description,
        content = content,
      )
      key -> Right(response)
    }
    Responses(
      responses = ListMap.from(entries),
    )
  }

  private def defaultDescription(key: ResponsesKey): String = key match {
    case ResponsesCodeKey(200) => "Successful response"
    case ResponsesCodeKey(201) => "Created"
    case ResponsesCodeKey(204) => "No content"
    case ResponsesCodeKey(400) => "Bad request"
    case ResponsesCodeKey(401) => "Unauthorized"
    case ResponsesCodeKey(403) => "Forbidden"
    case ResponsesCodeKey(404) => "Not found"
    case ResponsesCodeKey(409) => "Conflict"
    case ResponsesCodeKey(422) => "Unprocessable entity"
    case ResponsesCodeKey(500) => "Internal server error"
    case ResponsesCodeKey(n) => s"Response $n"
    case ResponsesDefaultKey => "Default response"
    case ResponsesRangeKey(r) => s"${r}xx response"
  }

  private def normalizePathVariables(ops: Seq[(String, ab.Operation)]): Seq[(String, ab.Operation)] = {
    val canonicalNames = mutable.Map[String, String]()
    ops.map { case (path, op) =>
      val segments = path.split("/", -1)
      val newSegments = segments.zipWithIndex.map { case (seg, idx) =>
        PathVariablePattern.findFirstMatchIn(seg) match {
          case Some(m) =>
            val prefix = segments
              .take(idx)
              .map {
                case PathVariablePattern(_) => "{}"
                case s => s
              }
              .mkString("/")
            val key = s"$idx:$prefix"
            val name = m.group(1)
            val canonical = canonicalNames.getOrElseUpdate(key, name)
            s"{$canonical}"
          case None => seg
        }
      }
      (newSegments.mkString("/"), op)
    }
  }

  private def toOpenApiPath(operationPath: String, resourcePath: Option[String]): String = {
    val fullPath = resourcePath match {
      case Some(rp) if operationPath.startsWith(rp) => operationPath
      case Some(rp) => rp + operationPath
      case None => operationPath
    }
    fullPath.replaceAll(":([a-zA-Z_][a-zA-Z0-9_]*)", "{$1}")
  }
}
