package scala.models.http4s

import scala.generator.{Namespaces, ScalaClientCommon, ScalaClientMethodConfig, ScalaClientMethodConfigs}
import lib.Text._

object Http4sScalaClientCommon extends ScalaClientCommon {

  def mustGetHttp4sConfig(config: ScalaClientMethodConfig): ScalaClientMethodConfigs.Http4s = {
    config match {
      case cfg: ScalaClientMethodConfigs.Http4s => cfg
      case _: ScalaClientMethodConfigs.Ning | _: ScalaClientMethodConfigs.Play => sys.error(s"Unexpected config: ${config.getClass.getName}")
    }
  }

  override def makeClientObject(
    config: ScalaClientMethodConfig
  ): String = {
    val extraMethods = config.extraClientObjectMethods match {
      case Some(methods) => methods.indentString(2) + "\n"
      case _ => ""
    }

    val http4sConfig = mustGetHttp4sConfig(config)

    s"""object Client {
        |  ${http4sConfig.asyncTypeImport}
        |
        |$extraMethods
        |  def parseJson[${http4sConfig.asyncTypeParam(Some(http4sConfig.asyncTypeConstraint)).map(p => p+", ").getOrElse("")}T](
        |    className: String,
        |    r: ${config.responseClass}
        |  )(implicit decoder: io.circe.Decoder[T]): ${http4sConfig.asyncType}[T] = r.attemptAs[T].${http4sConfig.monadTransformerInvoke}.flatMap {
        |    case ${http4sConfig.rightType}(value) => ${http4sConfig.asyncSuccessInvoke}(value)
        |    case ${http4sConfig.leftType}(error) => ${http4sConfig.wrappedAsyncType(config.asyncTypeConstraint).getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${Namespaces(config.namespace).errors}.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]", None, error))
        |  }
        |}""".stripMargin.trim
  }

}
