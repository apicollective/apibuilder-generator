package scala.models.http4s

import scala.generator.{Namespaces, ScalaClientCommon, ScalaClientMethodConfig, ScalaClientMethodConfigs}
import lib.Text._

object Http4sScalaClientCommon extends ScalaClientCommon {
  override def makeClientObject(
    config: ScalaClientMethodConfig
  ): String = {
    val extraMethods = config.extraClientObjectMethods match {
      case Some(methods) => methods.indent(2) + "\n"
      case _ => ""
    }

    val http4sConfig = config match {
      case cfg: ScalaClientMethodConfigs.Http4s => cfg
    }

    s"""object Client {
        |  ${http4sConfig.asyncTypeImport}
        |
        |$extraMethods
        |  def parseJson[${http4sConfig.asyncTypeParam(Some("Effect")).map(p => p+", ").getOrElse("")}T](
        |    className: String,
        |    r: ${config.responseClass}
        |  )(implicit decoder: io.circe.Decoder[T]): ${http4sConfig.asyncType}[T] = r.attemptAs[T].${http4sConfig.monadTransformerInvoke}.flatMap {
        |    case ${http4sConfig.rightType}(value) => ${http4sConfig.wrappedAsyncType("Sync").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncSuccess}(value)
        |    case ${http4sConfig.leftType}(error) => ${http4sConfig.wrappedAsyncType("Sync").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${Namespaces(config.namespace).errors}.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]", None, error))
        |  }
        |}""".stripMargin.trim
  }

}
