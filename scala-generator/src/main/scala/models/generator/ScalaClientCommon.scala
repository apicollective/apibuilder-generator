package scala.generator

object ScalaClientCommon {

  def apply(
    config: ScalaClientMethodConfig
  ): String = {
    Seq(
      ScalaClientObject(config),
      ScalaClientAuthClassses()
    ).mkString("\n\n")
  }

  def clientSignature(
    config: ScalaClientMethodConfig
  ): String = {
    val defaultUrl = config.baseUrl match {
      case None => ""
      case Some(url) => s" = ${ScalaUtil.wrapInQuotes(url)}"
    }

    s"""
class Client(
  ${if (config.expectsInjectedWsClient) "ws: play.api.libs.ws.WSClient,\n  "}val baseUrl: String$defaultUrl,
  auth: scala.Option[${config.namespace}.Authorization] = None,
  defaultHeaders: Seq[(String, String)] = Nil${config.extraClientCtorArgs.getOrElse("")}
) extends interfaces.Client
""".trim
  }

}
