package models.ning

import com.gilt.apidoc.generator.models.InvocationForm
import com.gilt.apidoc.spec.models.Service
import generator._
import models._
import lib.Text._

case class NingVersion(
  name: String
)

object NingVersions {

  val V1_8_x = NingVersion(
    name = "1.8.x"
  )

}

/**
 * Uses play JSON libraries for json
 * serialization/deserialization. Otherwise only depends on ning async
 * http client.
 */
object Ning18ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): String = {
    NingClientGenerator.invoke(NingVersions.V1_8_x, form)
  }

}

object NingClientGenerator {

  def invoke(version: NingVersion, form: InvocationForm): String = {
    NingClientGenerator(version, form).invoke()
  }

}

case class NingClientGenerator(
  version: NingVersion,
  form: InvocationForm
) {

  private val ssd = new ScalaService(form.service)

  def invoke(): String = {
    ApidocHeaders(form.userAgent).toJavaString + "\n" +
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

    val methodConfig = new ScalaClientMethodConfigs.Ning {
      override def namespace = ssd.namespace
    }
    val methodGenerator = ScalaClientMethodGenerator(methodConfig, ssd)

    s"""package ${ssd.namespace} {
  import com.ning.http.client.{AsyncCompletionHandler, AsyncHttpClient, Realm, Request, RequestBuilder, Response}

  object Client {
    def parseJson[T](r: com.ning.http.client.Response, f: (play.api.libs.json.JsValue => play.api.libs.json.JsResult[T])): T = {
      f(play.api.libs.json.Json.parse(r.getResponseBody("UTF-8"))) match {
        case play.api.libs.json.JsSuccess(x, _) => x
        case play.api.libs.json.JsError(errors) => {
          throw new FailedRequest(r, Some("Invalid json: " + errors.mkString(" ")))
        }
      }
    }
  }

  class Client(apiUrl: String, apiToken: scala.Option[String] = None) {
    import ${ssd.modelNamespace}.json._
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

      apiToken.fold(builder) { token =>
        builder.setRealm(
          new Realm.RealmBuilder()
            .setPrincipal(token)
            .setUsePreemptiveAuth(true)
            .setScheme(Realm.AuthScheme.BASIC)
            .build()
        )
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

${PathSegment.definition.indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}
}"""
  }

}
