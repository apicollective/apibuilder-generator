package scala.generator

import scala.models.{FeatureMigration, JsonImports}
import io.apibuilder.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import lib.Text._

class ScalaClientMethodGenerator(
  config: ScalaClientMethodConfig,
  ssd: ScalaService,
) {

  protected val namespaces: Namespaces = Namespaces(config.namespace)

  protected val generatorUtil: ScalaGeneratorUtil = new ScalaGeneratorUtil(config)

  protected val sortedResources: Seq[ScalaResource] = ssd.resources.sortWith { _.plural.toLowerCase < _.plural.toLowerCase }

  protected val featureMigration: FeatureMigration = FeatureMigration(ssd.service.apidoc.version)

  def traitsAndErrors(): String = {
    Seq(
      interfaces(),
      traits(),
      errorPackage()
    ).mkString("\n\n").trim
  }

  def methodName(resource: ScalaResource): String = {
    lib.Text.snakeToCamelCase(lib.Text.camelCaseToUnderscore(resource.plural).toLowerCase)
  }

  def accessors(): String = {
    sortedResources.map { resource =>
      s"def ${methodName(resource)}: ${resource.plural}${config.wrappedAsyncType().getOrElse("")} = ${resource.plural}"
    }.mkString("\n\n")
  }

  private[this] def responseType(resource: ScalaResource): String = {
    s"${namespaces.base}.${resource.plural}${config.wrappedAsyncType().getOrElse("")}"
  }

  private[this] def responseTypeWithEnvelope(resource: ScalaResource): String = {
    val name = responseType(resource)
    config.responseEnvelopeClassName match {
      case None => name
      case Some(envelopeName) => s"${namespaces.base}.$envelopeName[$name]"
    }
  }

  def interfaces(): String = {
    Seq(
      "package interfaces {",
      Seq(
        s"trait Client${config.asyncTypeParam().map(p => s"[$p]").getOrElse("")} {",
        "  def baseUrl: String",
        sortedResources.map { resource =>
          s"def ${methodName(resource)}: ${responseType(resource)}"
       }.mkString("\n").indentString(2),
        "}"
      ).mkString("\n").indentString(2),
      "}"
    ).mkString("\n\n")
  }

  def traits(): String = {
    sortedResources.map { resource =>
      s"trait ${resource.plural}${config.asyncTypeParam().map(p => s"[$p]").getOrElse("")} {\n" +
      methods(resource).map(_.interface).mkString("\n\n").indentString(2) +
      "\n}"
    }.mkString("\n\n")
  }

  def objects(): String = {
    sortedResources.map { resource =>
      Seq(
        Some(ScalaUtil.deprecationString(resource.deprecation).trim).filter(_.nonEmpty),
        Some(
          s"object ${resource.plural} extends ${resource.plural}${config.wrappedAsyncType().getOrElse("")} {\n" +
            methods(resource).map(_.code).mkString("\n\n").indentString(2) +
            "\n}"
        )
      ).flatten.mkString("\n")
    }.mkString("\n\n")
  }

  def errorPackage(): String = {
    Seq(
      Some("package errors {"),
      modelErrorClasses() match {
        case Nil => None
        case classes => Some {
          val jsonImports =
            if (includeJsonImportsInErrorsPackage) JsonImports(ssd.service).mkString("\n").indentString(2) + "\n\n"
            else ""
          jsonImports + classes.mkString("\n\n").indentString(2)
        }
      },
      Some(failedRequestClass().indentString(2)),
      Some("}")
    ).flatten.mkString("\n\n")
  }

  protected def failedRequestClass(): String = {
    """final case class FailedRequest(responseCode: Int, message: String, requestUri: Option[_root_.java.net.URI] = None) extends _root_.java.lang.Exception(s"HTTP $responseCode: $message")"""
  }

  /**
    * Returns custom case classes based on the service description for
    * all error return types. e.g. a 409 that returns Seq[Error] is
    * handled via these classes.
    */
  protected def modelErrorClasses(): Seq[String] = {
    ssd.resources.flatMap(_.operations).flatMap(_.responses).filter(r => !r.isSuccess).map { response =>
      errorTypeClass(response)
    }.distinct.sorted
  }

  protected def includeJsonImportsInErrorsPackage: Boolean = true

  protected def errorTypeClass(response: ScalaResponse): String = {
    require(!response.isSuccess)

    if (response.isUnit) {
      unitExceptionClass(response.errorClassName)
    } else {
      exceptionClass(
        response.errorClassName,
        response.errorVariableName.map { name =>
          val json = config.toJson("response", response.datatype.name)
          s"lazy val $name = ${json.indentString(2).trim}"
        }
      )
    }
  }

  protected def exceptionClass(
    className: String,
    body: Option[String] = None
  ): String = {
    val bodyString = body match {
      case None => ""
      case Some(b) => "{\n" + b.indentString(2) + "\n}"
    }

    Seq(
      s"final case class $className(",
      s"  response: ${config.responseClass},",
      s"  message: Option[String] = None",
      s""") extends Exception(message.getOrElse(response.${config.responseStatusMethod} + ": " + response.${config.responseBodyMethod}))$bodyString"""
    ).mkString("\n")

  }

  protected def unitExceptionClass(
    className: String
  ): String = {
    s"final case class $className(status: Int)" + """ extends Exception(s"HTTP $status")"""
  }

  def methods(resource: ScalaResource): Seq[ScalaClientMethod] = {
    resource.operations.map { op =>
      val path = generatorUtil.pathParams(op)

      val payload = generatorUtil.formBody(op, canSerializeUuid = config.canSerializeUuid)
      val queryParameters = generatorUtil.queryParameters("queryParameters", op.queryParameters)
      val headerParameters = generatorUtil.queryParameters("headerParameters", op.headerParameters)

      val code = new scala.collection.mutable.ListBuffer[String]()
      val args = new scala.collection.mutable.ListBuffer[String]()
      payload.foreach { v =>
        code.append(v)
        args.append("body = Some(payload)")
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

      val methodCall = code.toList match {
        case Nil => s"""_executeRequest("${op.method}", $path, ${args.mkString(", ")})"""
        case v => s"""${v.mkString("\n\n")}\n\n_executeRequest("${op.method}", $path, ${args.mkString(", ")})"""
      }

      val hasOptionResult = if (featureMigration.hasImplicit404s()) {
        op.responses.filter(_.isSuccess).find(_.isOption).map { _ =>
          s"\ncase r if r.${config.responseStatusMethod} == 404 => None"
        }
      } else {
        None
      }

      val allResponseCodes = (
        op.responses.flatMap { r =>
          r.code match {
            case ResponseCodeInt(value) => Some(value)
            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => None
          }
        } ++ (hasOptionResult match {
          case None => Seq.empty
          case Some(_) => Seq(404)
        })
      ).distinct.sorted

      val defaultResponse = op.responses.find { r =>
        r.code match {
          case ResponseCodeOption.Default => true
          case ResponseCodeOption.UNDEFINED(_) | ResponseCodeInt(_) | ResponseCodeUndefinedType(_) => false
        }
      } match {
        case Some(response) => {
          if (response.isUnit) {
            s"case r => throw ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod})"
          } else {
            s"case r => throw ${namespaces.errors}.${response.errorClassName}(r)"
          }
        }
        case None => {
          s"""case r => throw ${namespaces.errors}.FailedRequest(r.${config.responseStatusMethod}, s"Unsupported response code[""" + "${r." + config.responseStatusMethod + s"""}]. Expected: ${allResponseCodes.mkString(", ")}"${PlayScalaClientCommon.failedRequestUriParam(config)})"""
        }
      }

      val matchResponse: String = {
        op.responses.flatMap { response =>
          response.code match {
            case ResponseCodeInt(statusCode) => {
              if (response.isSuccess) {
                if (featureMigration.hasImplicit404s && response.isOption) {
                  if (response.isUnit) {
                    Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Some($unitResponse")
                  } else {
                    val result = config.buildResponse("r", response.datatype.name)
                    Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Some($result)")
                  }

                } else if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $unitResponse")

                } else {
                  val result = config.buildResponse("r", response.datatype.name)
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $result")
                }

              } else if (featureMigration.hasImplicit404s && response.isNotFound && response.isOption) {
                // will be added later
                None

              } else {
                if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => throw ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod})")

                } else {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => throw ${namespaces.errors}.${response.errorClassName}(r)")
                }
              }
            }

            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
              None
            }
          }
        }.mkString("\n")
      } + hasOptionResult.getOrElse("") + s"\n$defaultResponse\n"

      new ScalaClientMethod(
        operation = op,
        returnType = s"${config.asyncType}[${withEnvelope(op.resultType, isOption = hasOptionResult.isDefined)}]",
        methodCall = methodCall,
        response = matchResponse,
        implicitArgs = config.implicitArgs,
        responseEnvelopeName = config.responseEnvelopeClassName,
      )
    }
  }

  private[this] def unitResponse: String = {
    config.responseEnvelopeClassName match {
      case None => "()"
      case Some(envelopeName) => s"${envelopeName}Impl" + "(body = (), status = r.status, headers = ResponseHeaders(r.headers))"
    }
  }

  private[this] def withEnvelope(resultType: String, isOption: Boolean): String = {
    val finalType = config.responseEnvelopeClassName match {
      case None => resultType
      case Some(envName) => s"${ssd.namespaces.base}.$envName[$resultType]"
    }
    if (isOption) {
      s"_root_.scala.Option[$finalType]"
    } else {
      finalType
    }
  }

}

class ScalaClientMethod(
  val operation: ScalaOperation,
  returnType: String,
  methodCall: String,
  response: String,
  implicitArgs: Option[String],
  responseEnvelopeName: Option[String],
) {

  val name: String = operation.name

  val argList: Option[String] = operation.parameters.find(_.name.toLowerCase == "requestheaders") match {
    case Some(_) => operation.argList()
    case None => operation.argList(Seq("requestHeaders: Seq[(String, String)] = Nil"))
  }

  protected[this] val commentString: Option[String] = toOption(
    ScalaGeneratorUtil.scaladoc(
      operation.description,
      operation.parameters.map { p => (p.name, p.param.description) }
    )
  )

  val interface: String = {
    Seq(
      commentString,
      toOption(ScalaUtil.deprecationString(operation.deprecation)),
      Some(s"""def $name(${argList.getOrElse("")})${implicitArgs.getOrElse("")}: $returnType""")
    ).flatten.mkString("\n")
  }

  val code: String = {
    Seq(
      toOption(ScalaUtil.deprecationString(operation.deprecation)),
      Some(s"""override def $name(${argList.getOrElse("")})${implicitArgs.getOrElse("")}: $returnType = {
${methodCall.indentString()}.map {
${response.indentString(4)}
  }
}""")
    ).flatten.mkString("\n")
  }

  protected[this] def toOption(value: String): Option[String] = {
    value.trim match {
      case "" => None
      case c => Some(c)
    }
  }
}
