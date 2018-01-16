package scala.generator

import lib.Text._

object PlayScalaClientCommon extends ScalaClientCommon

class ScalaClientCommon {

  def apply(
    config: ScalaClientMethodConfig
  ): String = {
    Seq(
      makeClientObject(config),
      ScalaClientAuthClassses()
    ).mkString("\n\n")
  }

  def clientSignature(
    config: ScalaClientMethodConfig
  ): String = {
    s"""
class Client${config.asyncTypeParam}(
  ${if (config.expectsInjectedWsClient) "ws: play.api.libs.ws.WSClient,\n  " else ""}${config.formatBaseUrl(config.baseUrl)},
  auth: scala.Option[${config.namespace}.Authorization] = None,
  defaultHeaders: Seq[(String, String)] = Nil${config.extraClientCtorArgs.getOrElse("")}
) extends interfaces.Client
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
      case Some(methods) => methods.indent(2) + "\n"
      case _ => ""
    }

    s"""
object Client {
$extraMethods
  def parseJson[T](
    className: String,
    r: ${config.responseClass},
    f: (play.api.libs.json.JsValue => play.api.libs.json.JsResult[T])
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
