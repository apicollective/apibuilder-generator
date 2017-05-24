package scala.generator

import lib.Text._

trait ScalaClientMethodConfig {

  /**
    * Namespace in which the client is defined
    */
  def namespace: String

  /**
    * Base URL for the service if provided
    */
  def baseUrl: Option[String]

  /**
    * The code to generate to encode a variable into a path.
    */
  def pathEncode(value: String): String

  /**
    * The name of the method on the response providing the status code.
    */
  def responseStatusMethod: String

  /**
    * The name of the method on the response providing the request URI.
    */
  def requestUriMethod: Option[String]

  /**
    * The name of the method on the response providing the body.
    */
  def responseBodyMethod: String

  /**
    * The class name for the Response object.
    */
  def responseClass: String

  /** Whether or not play.api.libs.ws.WS exists as a static object, or if
    * play.api.libs.ws.WSClient is expected to be passed in.
   */
  def expectsInjectedWsClient: Boolean

  /**
    * Extra arguments that we need to provide to the Client, like e.g.
    * our own async http client
    */
  def extraClientCtorArgs: Option[String]

  /**
    * Extra methods to add to the Client object
    */
  def extraClientObjectMethods: Option[String]

  /**
    * true if the native libraries can serialized UUID. False
    * otherwise. If false, we map the UUIDs to strings in the
    * generated code.
    */
  def canSerializeUuid: Boolean

  /**
    * If client methods require a second (implicit) parameter list,
    * e.g. to pass in an ExecutionContext, this can be specified here.
    */
  def implicitArgs: Option[String]

  /**
    * Given a response and a class name, returns code to create an
    * instance of the specified class.
    */
  def toJson(responseName: String, className: String): String = {
    s"""_root_.${namespace}.Client.parseJson("$className", $responseName, _.validate[$className])"""
  }

  def clientObject(): String = {
    val extraMethods = extraClientObjectMethods match {
      case Some(methods) => methods.indent(2) + "\n"
      case _ => ""
    }

    s"""
    |object Client {
    |$extraMethods
    |  def parseJson[T](
    |    className: String,
    |    r: $responseClass,
    |    f: (play.api.libs.json.JsValue => play.api.libs.json.JsResult[T])
    |  ): T = {
    |    f(play.api.libs.json.Json.parse(r.$responseBodyMethod)) match {
    |      case play.api.libs.json.JsSuccess(x, _) => x
    |      case play.api.libs.json.JsError(errors) => {
    |        throw new ${Namespaces(namespace).errors}.FailedRequest(r.$responseStatusMethod, s"Invalid json for class[" + className + "]: " + errors.mkString(" ")${ScalaClientCommon.failedRequestUriParam(this)})
    |      }
    |    }
    |  }
    |
    |}""".stripMargin.trim
  }
}

object ScalaClientMethodConfigs {

  trait Play extends ScalaClientMethodConfig {
    override def pathEncode(value: String) = s"""play.utils.UriEncoding.encodePathSegment($value, "UTF-8")"""
    override val responseStatusMethod = "status"
    override val responseBodyMethod = "body"
    override val expectsInjectedWsClient = false
    override val extraClientCtorArgs = None
    override val extraClientObjectMethods = None
    override val implicitArgs = Some("(implicit ec: scala.concurrent.ExecutionContext)")
  }

  case class Play22(namespace: String, baseUrl: Option[String]) extends Play {
    override val responseClass = "play.api.libs.ws.Response"
    override val requestUriMethod = Some("ahcResponse.getUri")
    override val expectsInjectedWsClient = false
    override val canSerializeUuid = false
  }

  case class Play23(namespace: String, baseUrl: Option[String]) extends Play {
    override val responseClass = "play.api.libs.ws.WSResponse"
    override val requestUriMethod = None
    override val expectsInjectedWsClient = false
    override val canSerializeUuid = true
  }

  case class Play24(namespace: String, baseUrl: Option[String]) extends Play {
    override val responseClass = "play.api.libs.ws.WSResponse"
    override val requestUriMethod = None
    override val expectsInjectedWsClient = false
    override val canSerializeUuid = true
  }

  case class Play25(namespace: String, baseUrl: Option[String]) extends Play {
    override val responseClass = "play.api.libs.ws.WSResponse"
    override val requestUriMethod = None
    override val expectsInjectedWsClient = true
    override val canSerializeUuid = true
  }

  trait Ning extends ScalaClientMethodConfig {
    override def pathEncode(value: String) = s"""_root_.$namespace.PathSegment.encode($value, "UTF-8")"""
    override val responseStatusMethod = "getStatusCode"
    override val responseBodyMethod = """getResponseBody("UTF-8")"""
    override val responseClass = "_root_.com.ning.http.client.Response"
    override val extraClientCtorArgs = Some(",\n  asyncHttpClient: AsyncHttpClient = Client.defaultAsyncHttpClient")
    override val extraClientObjectMethods = Some("""
private lazy val defaultAsyncHttpClient = {
  new AsyncHttpClient(
    new AsyncHttpClientConfig.Builder()
      .setExecutorService(java.util.concurrent.Executors.newCachedThreadPool())
      .build()
  )
}
""")
    override val canSerializeUuid = true
    override val implicitArgs = Some("(implicit ec: scala.concurrent.ExecutionContext)")

    def addQueryParamMethod: String
  }

  case class Ning18(namespace: String, baseUrl: Option[String]) extends Ning {
    override def addQueryParamMethod: String = "addQueryParameter"
    override val requestUriMethod = Some("getUri")
    override val expectsInjectedWsClient = false
  }

  case class Ning19(namespace: String, baseUrl: Option[String]) extends Ning {
    override def addQueryParamMethod: String = "addQueryParam"
    override val requestUriMethod = Some("getUri.toJavaNetURI")
    override val expectsInjectedWsClient = false
  }

}
