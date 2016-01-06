package scala.models.ning

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
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
    val config = ScalaClientMethodConfigs.Ning18(Namespaces.quote(form.service.namespace))
    NingClientGenerator(config, form).invoke()
  }

}

object Ning19ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val config = ScalaClientMethodConfigs.Ning19(Namespaces.quote(form.service.namespace))
    NingClientGenerator(config, form).invoke()
  }

}

case class NingClientGenerator(
  config: ScalaClientMethodConfigs.Ning,
  form: InvocationForm
) {

  private[this] val ssd = new ScalaService(form.service)

  def invoke(): Either[Seq[String], Seq[File]] = {
    ScalaCaseClasses.modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(generateCode())
      case errors => Left(errors)
    }
  }

  private def generateCode(): Seq[File] = {
    val source = ApidocComments(form.service.version, form.userAgent).toJavaString + "\n" +
      Seq(
        Play2Models.generateCode(form, addBindables = false, addHeader = false).map(_.contents).mkString("\n\n"),
        client()
      ).mkString("\n\n")

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", source, Some("Scala")))
  }

  private[ning] def toJson(klass: String): String = {
    Seq(
      s"""play.api.libs.json.Json.parse(response.getResponseBody("UTF-8")).validate[${klass}] match {""",
      s"""  case play.api.libs.json.JsSuccess(x, _) => x""",
      s"""  case play.api.libs.json.JsError(errors) => sys.error("Invalid json: " + errors.mkString(" "))""",
      s"}"
    ).mkString("\n")
  }

  private def client(): String = {
    val headers = Headers(form)
    val headerString = headers.scala.map { case (name, value) =>
      s".addHeader(${ScalaUtil.wrapInQuotes(name)}, $value)"
    }.mkString("\n")

    val methodGenerator = ScalaClientMethodGenerator(config, ssd)

    s"""package ${ssd.namespaces.base} {
  import com.ning.http.client.{AsyncCompletionHandler, AsyncHttpClient, AsyncHttpClientConfig, Realm, Request, RequestBuilder, Response}

${headers.objectConstants.indent(2)}

${ScalaClientCommon.clientSignature(config).indent(2)} {
    import org.slf4j.Logger
    import org.slf4j.LoggerFactory
${JsonImports(form.service).mkString("\n").indent(4)}

    def closeAsyncHttpClient() {
      asyncHttpClient.close()
    }

    val logger = LoggerFactory.getLogger(getClass)

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    def _logRequest(request: Request) {
      logger.info("_logRequest: " + request)
    }

    def _requestBuilder(method: String, path: String): RequestBuilder = {
      val builder = new RequestBuilder(method)
        .setUrl(apiUrl + path)
${headerString.indent(8)}

      defaultHeaders.foreach { h => builder.addHeader(h._1, h._2) }

      auth.fold(builder) {
        case Authorization.Basic(username, passwordOpt) => {
          builder.setRealm(
            new Realm.RealmBuilder()
              .setPrincipal(username)
              .setPassword(passwordOpt.getOrElse(""))
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
      queryParameters: Seq[(String, String)] = Seq.empty,
      body: Option[play.api.libs.json.JsValue] = None
    )(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[com.ning.http.client.Response] = {
      val request = _requestBuilder(method, path)

      queryParameters.foreach { pair =>
        request.${config.addQueryParamMethod}(pair._1, pair._2)
      }

      val requestWithParamsAndBody = body.fold(request) { b =>
        val serialized = play.api.libs.json.Json.stringify(b)
        request.setBody(serialized).addHeader("Content-type", "application/json; charset=UTF-8")
      }

      val finalRequest = requestWithParamsAndBody.build()
      _logRequest(finalRequest)

      val result = scala.concurrent.Promise[com.ning.http.client.Response]()
      asyncHttpClient.executeRequest(finalRequest,
        new AsyncCompletionHandler[Unit]() {
          override def onCompleted(r: com.ning.http.client.Response) = result.success(r)
          override def onThrowable(t: Throwable) = result.failure(t)
        }
      )
      result.future
    }

  }

${ScalaClientCommon(config).indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}

${PathSegment.definition.indent(2)}
}"""
  }

}
