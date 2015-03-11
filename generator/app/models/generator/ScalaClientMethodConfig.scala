package generator

trait ScalaClientMethodConfig {

  /**
    * Namespace in which the client is defined
    */
  def namespace: String

  /**
    * The name of the method to call to encode a variable into a path.
    */
  def pathEncodingMethod: String

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

  /**
    * true if we need to provide our own async http client (in which
    * case we add a default shared executor and also expose the http
    * client in the API directly to be overridden as necessary)
    */
  def requiresAsyncHttpClient: Boolean

  /**
    * Given a response and a class name, returns code to create an
    * instance of the specified class.
    */
  def toJson(responseName: String, className: String): String = {
    s"""_root_.${namespace}.Client.parseJson("$className", $responseName, _.validate[$className])"""
  }

  /**
   * Given an accessor method name and a type, returns code to create an
   * accessor var.
   */
  def accessor(methodName: String, typeName: String): String = {
    s"def ${methodName}: ${typeName} = ${typeName}"
  }

}

object ScalaClientMethodConfigs {

  trait Play extends ScalaClientMethodConfig {
    override val pathEncodingMethod = "play.utils.UriEncoding.encodePathSegment"
    override val responseStatusMethod = "status"
    override val responseBodyMethod = "body"
    override val requiresAsyncHttpClient = false
  }

  case class Play22(namespace: String) extends Play {
    override val responseClass = "play.api.libs.ws.Response"
    override val requestUriMethod = Some("ahcResponse.getUri")
  }

  case class Play23(namespace: String) extends Play {
    override val responseClass = "play.api.libs.ws.WSResponse"
    override val requestUriMethod = None
  }

  trait Ning extends ScalaClientMethodConfig {
    override val pathEncodingMethod = s"_root_.${namespace}.PathSegment.encode"
    override val responseStatusMethod = "getStatusCode"
    override val responseBodyMethod = """getResponseBody("UTF-8")"""
    override val responseClass = "_root_.com.ning.http.client.Response"
    override val requiresAsyncHttpClient = true

    def addQueryParamMethod: String
  }

  case class Ning18(namespace: String) extends Ning {
    override def addQueryParamMethod: String = "addQueryParameter"
    override val requestUriMethod = Some("getUri")
  }

  case class Ning19(namespace: String) extends Ning {
    override def addQueryParamMethod: String = "addQueryParam"
    override val requestUriMethod = Some("getUri.toJavaNetURI")
  }

}
