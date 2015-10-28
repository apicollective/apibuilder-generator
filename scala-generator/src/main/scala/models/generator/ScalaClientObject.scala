package scala.generator

import lib.Text._

object ScalaClientObject {

  def failedRequestUriParam(config: ScalaClientMethodConfig): String = {
    config.requestUriMethod match {
      case None => ""
      case Some(methodName) => s", requestUri = Some(r.$methodName)"
    }
  }

  def apply(
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
        throw new ${Namespaces(config.namespace).errors}.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]: " + errors.mkString(" ")${ScalaClientObject.failedRequestUriParam(config)})
      }
    }
  }

}
""".trim
  }

}
