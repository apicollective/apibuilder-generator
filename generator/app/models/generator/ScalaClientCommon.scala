package generator

object ScalaClientCommon {

  def apply(
    config: ScalaClientMethodConfig
  ): String = {
    Seq(
      ScalaClientJsonParser(config),
      ScalaClientAuthClassses()
    ).mkString("\n\n")
  }

  def clientSignature(
    config: ScalaClientMethodConfig
  ): String = {
    s"class Client(apiUrl: String, auth: scala.Option[${config.namespace}.Authorization] = None)"
  }

}
