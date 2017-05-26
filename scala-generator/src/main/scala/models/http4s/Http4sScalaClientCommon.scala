package scala.models.http4s

import scala.generator.{Namespaces, ScalaClientCommon, ScalaClientMethodConfig, ScalaUtil, ScalaClientAuthClassses}
import lib.Text._

object Http4sScalaClientCommon extends ScalaClientCommon {
  override def makeClientObject(
    config: ScalaClientMethodConfig
  ): String = {
    val extraMethods = config.extraClientObjectMethods match {
      case Some(methods) => methods.indent(2) + "\n"
      case _ => ""
    }

    s"""object Client {
        |  import scalaz._
        |  import scalaz.concurrent.Task
        |
        |$extraMethods
        |  def parseJson[T](
        |    className: String,
        |    r: ${config.responseClass}
        |  )(implicit decoder: org.http4s.EntityDecoder[T]): Task[T] = r.attemptAs[T].run.flatMap {
        |    case \\/-(value) => Task.now(value)
        |    case -\\/(error) => Task.fail(new ${Namespaces(config.namespace).errors}.FailedRequest(r.${config.responseStatusMethod}, s"Invalid json for class[" + className + "]", None, error))
        |  }
        |}""".stripMargin.trim
  }

}
