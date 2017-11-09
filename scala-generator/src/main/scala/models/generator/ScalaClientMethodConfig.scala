package scala.generator

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

  def asyncType: String

  def asyncSuccess: String

  def formatBaseUrl(url: Option[String]): String = s"val baseUrl: String" + url.fold("")(u => s" = ${ScalaUtil.wrapInQuotes(u)}")

  /**
    * Given a response and a class name, returns code to create an
    * instance of the specified class.
    */
  def toJson(responseName: String, className: String): String = {
    s"""_root_.${namespace}.Client.parseJson("$className", $responseName, _.validate[$className])"""
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
    override val asyncType: String = "scala.concurrent.Future"
    override val asyncSuccess: String = "successful"
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

  case class Play26(namespace: String, baseUrl: Option[String]) extends Play {
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
    override val asyncType: String = "scala.concurrent.Future"
    override val asyncSuccess: String = "successful"

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


  trait Http4s extends ScalaClientMethodConfig {
    override def pathEncode(value: String) = value
    override val responseStatusMethod = "status.code"
    override val responseBodyMethod = """body"""
    override val responseClass = "org.http4s.Response"
    override val extraClientCtorArgs = Some(",\n  asyncHttpClient: org.http4s.client.Client = Client.defaultAsyncHttpClient")
    override val extraClientObjectMethods = Some("""
implicit def circeJsonDecoder[A](implicit decoder: io.circe.Decoder[A]) = org.http4s.circe.jsonOf[A]

private lazy val defaultAsyncHttpClient = PooledHttp1Client()
""")
    override val canSerializeUuid = true
    override val implicitArgs = None
    override val asyncType: String = "scalaz.concurrent.Task"
    override val asyncSuccess: String = "now"
    override val requestUriMethod = None //Some("getUri.toJavaNetURI")
    override val expectsInjectedWsClient = false
    override def formatBaseUrl(url: Option[String]): String = s"val baseUrl: org.http4s.Uri" + url.fold("")(u => s" = org.http4s.Uri.unsafeFromString(${ScalaUtil.wrapInQuotes(u)})")
    override def toJson(responseName: String, className: String): String = {
      s"""_root_.${namespace}.Client.parseJson[$className]("$className", $responseName)"""
    }
    def leftType: String
    def rightType: String
    def monadTransformerInvoke: String
  }

  case class Http4s015(namespace: String, baseUrl: Option[String]) extends Http4s {
    override val asyncType = "scalaz.concurrent.Task"
    override val leftType = "scalaz.-\\/"
    override val rightType = "scalaz.\\/-"
    override val monadTransformerInvoke = "run"
  }

  case class Http4s017(namespace: String, baseUrl: Option[String]) extends Http4s {
    override val asyncType = "fs2.Task"
    override val leftType = "Left"
    override val rightType = "Right"
    override val monadTransformerInvoke = "value"
  }

  case class Http4s018(namespace: String, baseUrl: Option[String]) extends Http4s {
    override val asyncType = "cats.effect.IO"
    override val leftType = "Left"
    override val rightType = "Right"
    override val monadTransformerInvoke = "value"
    override val responseClass = "org.http4s.Response[IO]"
    override val extraClientCtorArgs = Some(",\n  asyncHttpClient: org.http4s.client.Client[IO] = Client.defaultAsyncHttpClient")
    override val extraClientObjectMethods = Some("""
implicit def circeJsonDecoder[A](implicit decoder: io.circe.Decoder[A]) = org.http4s.circe.jsonOf[IO, A]

private lazy val defaultAsyncHttpClient = PooledHttp1Client[IO]()
""")
    override val asyncSuccess: String = "pure"
  }
}
