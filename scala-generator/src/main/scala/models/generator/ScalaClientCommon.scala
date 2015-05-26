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
    
    val executorService = config.requiresAsyncHttpClient match {
      case true => ",\n  asyncHttpClient: AsyncHttpClient = Client.defaultAsyncHttpClient"
      case false => ""
    }

    s"""
class Client(
  apiUrl: String,
  auth: scala.Option[${config.namespace}.Authorization] = None,
  defaultHeaders: Seq[(String, String)] = Nil$executorService
)
""".trim
  }

}
