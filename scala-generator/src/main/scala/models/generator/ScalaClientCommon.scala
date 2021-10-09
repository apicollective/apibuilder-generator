package scala.generator

import lib.Text._

object PlayScalaClientCommon extends ScalaClientCommon

class ScalaClientCommon {

  def apply(
    config: ScalaClientMethodConfig
  ): String = {
    Seq(
      makeClientObject(config),
      ScalaClientAuthClasses()
    ).mkString("\n\n")
  }

  def clientSignature(
    config: ScalaClientMethodConfig
  ): String = {
    s"""
class Client${config.asyncTypeParam(Some("Sync")).map(p => s"[$p]").getOrElse("")}(
  ${if (config.expectsInjectedWsClient) "ws: play.api.libs.ws.WSClient,\n  " else ""}${config.formatBaseUrl(config.baseUrl)},
  auth: scala.Option[${config.namespace}.Authorization] = None,
  defaultHeaders: Seq[(String, String)] = Nil${config.extraClientCtorArgs.getOrElse("")}
) extends interfaces.Client${config.wrappedAsyncType().getOrElse("")}
""".trim
  }

  def responseEnvelopeTrait(name: String): String = {
    s"""
trait $name[T] {
  def body: T
  def status: Int
  def headers: ResponseHeaders
}

case class ${name}Impl[T](
  override val body: T,
  override val status: Int,
  override val headers: ResponseHeaders,
) extends $name[T]

case class ResponseHeaders(all: Map[String, scala.collection.Seq[String]]) {
  def get(name: String): _root_.scala.Option[String] = getAll(name).headOption
  def getAll(name: String): _root_.scala.collection.Seq[String] = all.getOrElse(name, Nil)
}
""".trim
  }

  def failedRequestUriParam(config: ScalaClientMethodConfig): String = {
    config.requestUriMethod match {
      case None => ""
      case Some(methodName) => s", requestUri = Some(r.$methodName)"
    }
  }

  def makeClientObject(
    config: ScalaClientMethodConfig
  ): String = {
    val extraMethods = config.extraClientObjectMethods match {
      case Some(methods) => methods.indentString(2) + "\n"
      case _ => ""
    }

    val parseJsonSignature = Seq(
      "className: String,",
      s"r: ${config.responseClass},",
      "f: (play.api.libs.json.JsValue => play.api.libs.json.JsResult[T])"
    ).mkString("\n").indentString(4)

    val buildResponse = config.responseEnvelopeClassName match {
      case None => ""
      case Some(envelopeName) =>
        s"""
           |def buildResponse[T](
           |$parseJsonSignature
           |): $envelopeName[T] = {
           |  ResponseImpl(
           |    body = parseJson(className, r, f),
           |    status = r.status,
           |    headers = ResponseHeaders(r.headers),
           |  )
           |}
           |""".stripMargin.indentString(2) + "\n\n"
    }

    s"""
object Client {
$extraMethods
$buildResponse  def parseJson[T](
$parseJsonSignature
  ): T = {
    f(play.api.libs.json.Json.parse(r.${config.responseBodyMethod})) match {
      case play.api.libs.json.JsSuccess(x, _) => x
      case play.api.libs.json.JsError(errors) => {
        throw ${Namespaces(config.namespace).errors}.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]: " + errors.mkString(" ")${failedRequestUriParam(config)})
      }
    }
  }

}
""".trim
  }
}
