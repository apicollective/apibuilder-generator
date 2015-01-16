package generator

object ScalaClientJsonParser {

  def apply(
    config: ScalaClientMethodConfig
  ): String = {
    s"""
object Client {
  def parseJson[T](
    className: String,
    r: ${config.responseClass},
    f: (play.api.libs.json.JsValue => play.api.libs.json.JsResult[T])
  ): T = {
    f(play.api.libs.json.Json.parse(r.${config.responseBodyMethod})) match {
      case play.api.libs.json.JsSuccess(x, _) => x
      case play.api.libs.json.JsError(errors) => {
        throw new ${config.namespace}.error.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]: " + errors.mkString(" "))
      }
    }
  }
}
""".trim
  }

}
