package models.http4s018

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
        |  import ${config.asyncType}
        |
        |$extraMethods
        |  def parseJson[T](
        |    className: String,
        |    r: ${config.responseClass}
        |  )(implicit decoder: io.circe.Decoder[T]): IO[T] = r.attemptAs[T].${http4sConfig.monadTransformerInvoke}.flatMap {
        |    case ${http4sConfig.rightType}(value) => IO.pure(value)
        |    case ${http4sConfig.leftType}(error) => IO.raiseError(new ${Namespaces(config.namespace).errors}.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]", None, error))
        |  }
        |}""".stripMargin.trim
  }

}
