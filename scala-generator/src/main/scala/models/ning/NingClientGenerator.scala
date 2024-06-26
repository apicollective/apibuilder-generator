package scala.models.ning

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import scala.generator._
import scala.models._
import lib.Text._
import lib.generator.CodeGenerator
import generator.ServiceFileNames

/**
 * Uses play JSON libraries for json
 * serialization/deserialization. Otherwise only depends on ning async
 * http client.
 */
object Ning18ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val config = ScalaClientMethodConfigs.Ning18(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig, form.service.baseUrl)
    NingClientGenerator(config, form).invoke()
  }

}

object Ning19ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val config = ScalaClientMethodConfigs.Ning19(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig, form.service.baseUrl)
    NingClientGenerator(config, form).invoke()
  }

}

object AsyncHttpClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val config = ScalaClientMethodConfigs.AsyncHttpClient(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig, form.service.baseUrl)
    NingClientGenerator(config, form).invoke()
  }

}

case class NingClientGenerator(
  config: ScalaClientMethodConfigs.Ning,
  form: InvocationForm
) {

  private val ssd = new ScalaService(form.service, Attributes.PlayDefaultConfig.withAttributes(form.attributes))

  def invoke(): Either[Seq[String], Seq[File]] = {
    Right(generateCode())
  }

  private def generateCode(): Seq[File] = {
    val source = ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n" +
      Seq(
        Play2ModelsScala2.generateCode(form, addBindables = false, addHeader = false, useBuiltInImplicits = false).map(_.contents).mkString("\n\n"),
        client()
      ).mkString("\n\n")

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", source, Some("Scala")))
  }

  private def client(): String = {
    val headers = Headers(form)
    val headerString = headers.scala.map { case (name, value) =>
      s".addHeader(${ScalaUtil.wrapInQuotes(name)}, $value)"
    }.mkString("\n")

    val methodGenerator = new ScalaClientMethodGenerator(config, ssd)

    s"""package ${ssd.namespaces.base} {
  import ${config.ningPackage}.{AsyncCompletionHandler, AsyncHttpClient, Realm, Request, RequestBuilder, Response${config.additionalImports}}

${headers.objectConstants.indentString(2)}

${PlayScalaClientCommon.clientSignature(config).indentString(2)} {
    import org.slf4j.{Logger, LoggerFactory}
${JsonImports(form.service).mkString("\n").indentString(4)}

    def closeAsyncHttpClient(): Unit = {
      asyncHttpClient.close()
    }

    val logger = LoggerFactory.getLogger(getClass)

${methodGenerator.accessors().indentString(4)}

${methodGenerator.objects().indentString(4)}

    def _logRequest(request: Request): Unit = {
      if (logger.isInfoEnabled) {
        logger.info("_logRequest: " + request)
      }
    }

    def _logResponse(response: Response): Unit = {
      if (logger.isInfoEnabled) {
        logger.info("_logResponse: status=" + response.getStatusCode + ", responseBody: " + response.${config.responseBodyMethod})
      }
    }

    def _requestBuilder(method: String, path: String, requestHeaders: Seq[(String, String)]): RequestBuilder = {
      val builder = new RequestBuilder(method)
        .setUrl(baseUrl + path)
${headerString.indentString(8)}

      defaultHeaders.foreach { h => builder.addHeader(h._1, h._2) }
      requestHeaders.foreach { h => builder.addHeader(h._1, h._2) }

      auth.fold(builder) {
        case Authorization.Basic(username, passwordOpt) => {
          builder.setRealm(
${config.realmBuilder("username", """passwordOpt.getOrElse("")""").indentString(12)}
              .setUsePreemptiveAuth(true)
              .setScheme(Realm.AuthScheme.BASIC)
              .build()
          )
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }
    }

    def _executeRequest(
      method: String,
      path: String,
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[play.api.libs.json.JsValue] = None
    )(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[${config.ningPackage}.Response] = {
      val allHeaders = body match {
        case None => requestHeaders
        case Some(_) => _withJsonContentType(requestHeaders)
      }

      val request = _requestBuilder(method, path, allHeaders)

      queryParameters.foreach { pair =>
        request.${config.addQueryParamMethod}(pair._1, pair._2)
      }

      val requestWithParamsAndBody = body.fold(request) { b =>
        val serialized = play.api.libs.json.Json.stringify(b)
        request.setBody(serialized)
      }

      val finalRequest = requestWithParamsAndBody.build()
      _logRequest(finalRequest)

      val result = scala.concurrent.Promise[${config.ningPackage}.Response]()
      asyncHttpClient.executeRequest(finalRequest,
        new AsyncCompletionHandler[Unit]() {
          override def onCompleted(r: ${config.ningPackage}.Response) = {
            _logResponse(r)
            result.success(r)
          }
          override def onThrowable(t: Throwable) = result.failure(t)
        }
      )
      result.future
    }

    /**
     * Adds a Content-Type: application/json header unless the specified requestHeaders
     * already contain a Content-Type header
     */
    def _withJsonContentType(headers: Seq[(String, String)]): Seq[(String, String)] = {
      headers.find { _._1.toUpperCase == "CONTENT-TYPE" } match {
        case None => headers ++ Seq(("Content-Type" -> "application/json; charset=UTF-8"))
        case Some(_) => headers
      }
    }

  }

${PlayScalaClientCommon(config).indentString(2)}

${methodGenerator.traitsAndErrors().indentString(2)}

${PathSegment.definition.indentString(2)}
}"""
  }

}
