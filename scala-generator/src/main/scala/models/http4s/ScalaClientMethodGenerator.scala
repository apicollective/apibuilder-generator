package scala.models.http4s

import io.apibuilder.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

import scala.generator.*

class ScalaClientMethodGenerator (
  config: ScalaClientMethodConfig,
  ssd: ScalaService
)  extends scala.generator.ScalaClientMethodGenerator(config, ssd) {
  import lib.Text.*

  val http4sConfig: ScalaClientMethodConfigs.Http4s = Http4sScalaClientCommon.mustGetHttp4sConfig(config)

  override protected val generatorUtil = new scala.models.http4s.ScalaGeneratorUtil(config)

  override def interfaces(): String = {
    Seq(
      "package interfaces {",
      Seq(
        s"trait Client${config.asyncTypeParam().map(p => s"[$p]").getOrElse("")} {",
        "  def baseUrl: org.http4s.Uri",
        sortedResources.map { resource =>
          s"def ${methodName(resource)}: ${namespaces.base}.${resource.plural}${config.wrappedAsyncType().getOrElse("")}"
        }.mkString("\n").indentString(2),
        "}"
      ).mkString("\n").indentString(2),
      "}"
    ).mkString("\n\n")
  }

  override protected def failedRequestClass(): String = {
    """final case class FailedRequest(responseCode: Int, message: String, requestUri: Option[_root_.java.net.URI] = None, parent: Exception = null) extends _root_.java.lang.Exception(s"HTTP $responseCode: $message", parent)"""
  }

  override def methods(resource: ScalaResource): Seq[ScalaClientMethod] = {
    resource.operations.map { op =>
      val payload = generatorUtil.formBody(op, canSerializeUuid = config.canSerializeUuid)
      val queryParameters = generatorUtil.queryParameters("queryParameters", op.queryParameters)
      val headerParameters = generatorUtil.queryParameters("headerParameters", op.headerParameters)

      val code = new scala.collection.mutable.ListBuffer[String]()
      val args = new scala.collection.mutable.ListBuffer[String]()

      code.append(s"val urlPath = ${generatorUtil.pathParams(op)}")
      args.append("path = urlPath")

      payload.foreach { v =>
        code.append(v)
        args.append("body = payload, formBody = formPayload")
      }

      queryParameters.foreach { v =>
        code.append(v)
        args.append("queryParameters = queryParameters")
      }
      headerParameters.fold(
        args.append("requestHeaders = requestHeaders")
      ){ h =>
        code.append(h)
        args.append("requestHeaders = (requestHeaders ++ headerParameters)")
      }

      val reqType = op.body.fold("Unit")(b => b.datatype.name)
      val resType: String = op.resultType

      val methodCall = code.toList match {
        case Nil => s"""_executeRequest[$reqType, $resType]("${op.method}", ${args.mkString(", ")})"""
        case v => s"""${v.mkString("\n\n")}\n\n_executeRequest[$reqType, $resType]("${op.method}", ${args.mkString(", ")})"""
      }

      val allResponseCodes = op.responses.flatMap { r =>
        r.code match {
          case ResponseCodeInt(value) => Some(value)
          case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => None
        }
      }.distinct.sorted

      val raiseFailure = s"${http4sConfig.wrappedAsyncType(config.asyncTypeConstraint).getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}"

      val defaultResponse = op.responses.find { r =>
        r.code match {
          case ResponseCodeOption.Default => true
          case ResponseCodeOption.UNDEFINED(_) | ResponseCodeInt(_) | ResponseCodeUndefinedType(_) => false
        }
      } match {
        case Some(response) =>
          if (response.isUnit) {
            s"case r => $raiseFailure(new ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod}))"
          } else {
            config match {
              case _: ScalaClientMethodConfigs.Http4s017 | _: ScalaClientMethodConfigs.Http4s015 =>
                s"case r => $raiseFailure(new ${namespaces.errors}.${response.errorClassName}(r))"
              case _ =>
                val json = config.toJson("r", response.datatype.name)
                s"case r => $json.flatMap(body => $raiseFailure(new ${namespaces.errors}.${response.errorClassName}(r.headers, r.status.code, None, body)))"
            }
          }
        case None =>
          s"""case r => $raiseFailure(new ${namespaces.errors}.FailedRequest(r.${config.responseStatusMethod}, s"Unsupported response code[""" + "${r." + config.responseStatusMethod + s"""}]. Expected: ${allResponseCodes.mkString(", ")}"${Http4sScalaClientCommon.failedRequestUriParam(config)}))"""
      }

      val matchResponse: String = {
        op.responses.flatMap { response =>
          response.code match {
            case ResponseCodeInt(statusCode) => {
              if (response.isSuccess) {
                if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => ${http4sConfig.asyncSuccessInvoke}(())")

                } else {
                  val json = config.toJson("r", response.datatype.name)
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $json")
                }
              } else {
                val raiseFailure = s"${http4sConfig.wrappedAsyncType(config.asyncTypeConstraint).getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}"

                if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $raiseFailure(new ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod}))")
                } else {
                  config match {
                    case _: ScalaClientMethodConfigs.Http4s017 | _: ScalaClientMethodConfigs.Http4s015 =>
                      Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $raiseFailure(new ${namespaces.errors}.${response.errorClassName}(r))")
                    case _ =>
                      val json = config.toJson("r", response.datatype.name)
                      Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $json.flatMap(body => $raiseFailure(new ${namespaces.errors}.${response.errorClassName}(r.headers, r.status.code, None, body)))")
                  }
                }
              }
            }

            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
              None
            }
          }
        }.mkString("\n")
      } + s"\n$defaultResponse\n"

      new ScalaClientMethod(
        operation = op,
        returnType = s"${config.asyncType}[$resType]",
        methodCall = methodCall,
        response = matchResponse,
        implicitArgs = config.implicitArgs,
      )
    }
  }

  private def errorClassesHttp4sLatest(): Seq[String] =
    ssd.resources.flatMap(_.operations).flatMap(_.responses).filter(r => !r.isSuccess).map { response =>
      require(!response.isSuccess)
      if (response.isUnit) {
        unitExceptionClass(response.errorClassName)
      } else {
        val variableName = response.errorVariableName
        val className = response.errorClassName
        val responseDataType = response.datatype.name

        val body = variableName.map(v => s"lazy val $v = body").getOrElse("")

        Seq(
          s"final case class $className(",
          s"  headers: org.http4s.Headers,",
          s"  status: Int,",
          s"  message: Option[String] = None" + variableName.map(_ => s",\n  body: $responseDataType").getOrElse(""),
          s""") extends Exception(s"HTTP $$status$${message.fold("")(m => s": $$m")}"){""",
          s"  $body",
          s"}"

        ).mkString("\n")
      }
    }.distinct.sorted

  override protected def modelErrorClasses(): Seq[String] =
    config match {
      case _:ScalaClientMethodConfigs.Http4s017 | _:ScalaClientMethodConfigs.Http4s015 => super.modelErrorClasses()
      case _ => errorClassesHttp4sLatest()
    }

  override protected def includeJsonImportsInErrorsPackage: Boolean = {
    config match {
      case _:ScalaClientMethodConfigs.Http4s017 | _:ScalaClientMethodConfigs.Http4s015 => true
      case _ => false
    }
  }
}

class ScalaClientMethod(
  operation: ScalaOperation,
  returnType: String,
  methodCall: String,
  response: String,
  implicitArgs: Option[String],
) extends scala.generator.ScalaClientMethod(operation, returnType, methodCall, response, implicitArgs, responseEnvelopeName = None) {
  import lib.Text.*

  override val interface: String = {
    Seq(
      commentString,
      toOption(ScalaUtil.deprecationString(operation.deprecation)),
      Some(s"""def $name(${argList.getOrElse("")})${implicitArgs.getOrElse("")}: $returnType""")
    ).flatten.mkString("\n")
  }

  override val code: String = {
    s"""${ScalaUtil.deprecationString(operation.deprecation)}override def $name(${argList.getOrElse("")})${implicitArgs.getOrElse("")}: $returnType = {
${methodCall.indentString()} {
${response.indentString(4)}
  }
}"""
  }
}
