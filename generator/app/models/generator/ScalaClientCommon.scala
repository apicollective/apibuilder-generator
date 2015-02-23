package generator

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
    
    val executorService = config.requiresExecutorService match {
      case true => ",\n  executorService: java.util.concurrent.ExecutorService = Client.defaultExecutorService"
      case false => ""
    }

    s"""
class Client(
  apiUrl: String,
  auth: scala.Option[${config.namespace}.Authorization] = None$executorService
)
""".trim
  }

}
