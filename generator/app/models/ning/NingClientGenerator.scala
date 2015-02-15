package models.ning

import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.Service
import generator._
import models._
import lib.Text._

/**
 * Uses play JSON libraries for json
 * serialization/deserialization. Otherwise only depends on ning async
 * http client.
 */
object Ning18ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): String = {
    val config = ScalaClientMethodConfigs.Ning(Namespaces.quote(form.service.namespace))
    NingClientGenerator(config, form).invoke()
  }

}

case class NingClientGenerator(
  config: ScalaClientMethodConfig,
  form: InvocationForm
) {

  private val ssd = new ScalaService(form.service)

  def invoke(): String = {
    ApidocComments(form.service.version, form.userAgent).toJavaString + "\n" +
    Seq(
      Play2Models(form, addHeader = false),
      client()
    ).mkString("\n\n")
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
    val headerString = (ssd.defaultHeaders ++ Seq(ScalaHeader("User-Agent", "UserAgent"))).map { h =>
      s""".addHeader("${h.name}", ${h.quotedValue}"""
    }.mkString("\n")

    val methodGenerator = ScalaClientMethodGenerator(config, ssd)

    s"""package ${ssd.namespaces.base} {
  import com.ning.http.client.{AsyncCompletionHandler, AsyncHttpClient, Realm, Request, RequestBuilder, Response}

  
  ${ScalaClientCommon.clientSignature(config)} {
    import ${ssd.namespaces.models}.json._
    import org.slf4j.Logger
    import org.slf4j.LoggerFactory

    val logger = LoggerFactory.getLogger(getClass)

    val asyncHttpClient = new AsyncHttpClient()
    private val UserAgent = "${form.userAgent.getOrElse("unknown")}"

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    def _logRequest(request: Request) {
      logger.info("_logRequest: " + request)
    }

    def _requestBuilder(method: String, path: String): RequestBuilder = {
      val builder = new RequestBuilder(method)
        .setUrl(apiUrl + path)
        .addHeader("User-Agent", UserAgent)

      auth.fold(builder) { a =>
        a match {
          case Authorization.Basic(username, password) => {
            builder.setRealm(
              new Realm.RealmBuilder()
                .setPrincipal(username)
                .setUsePreemptiveAuth(true)
                .setScheme(Realm.AuthScheme.BASIC)
                .build()
            )
          }
          case _ => sys.error("Invalid authorization scheme[" + a.getClass + "]")
        }
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
        request.addQueryParameter(pair._1, pair._2)
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
