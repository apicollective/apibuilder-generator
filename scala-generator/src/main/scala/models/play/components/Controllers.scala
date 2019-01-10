package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import io.apibuilder.spec.v0.models._
import scala.generator._
import scala.models.play.Helpers._

class Controllers(service: ScalaService) extends Component {

  def function(operation: ScalaOperation): String = {
    val contentType = operation.body.fold("AnyContent")(_.datatype.name)
    s"""def ${operation.name}(${operation.argList().getOrElse("")}): F[${resultType(operation)}]"""
  }

  def resultType(operation: ScalaOperation): String =
    s"${operation.name.capitalize}Result"

  def resultOption(`type`: String, response: ScalaResponse): Option[String] = {
    val responseWithCode = response.code match {
      case ResponseCodeInt(code) => (response, code).some
      case _ => None
    }
    responseWithCode.map {
      case (r, c) if r.isUnit => s"""case object HTTP${c} extends ${`type`}"""
      case (r, c) => s"""case class HTTP${c}(value: ${r.resultType}) extends ${`type`}"""
    }
  }

  def result(operation: ScalaOperation): String = {
    val `type` = resultType(operation)
    val resultOptions = operation.responses.flatMap(resultOption(`type`, _))

    s"""
    |sealed trait ${`type`}
    |object ${`type`} {
    |  ${resultOptions.mkString("\n").addMargin(2)}
    |}
  """
  }

  def controller(resource: ScalaResource): String = {
    val name = ScalaUtil.toClassName(resource.resource.plural)
    val results = resource.operations.map(result)
    val functions = resource.operations.map(function)

    s"""
      |trait ${name}Controller[F[_]] {
      |
      |  ${results.mkString("\n").addMargin(2)}
      |
      |  ${functions.mkString("\n").addMargin(2)}
      |
      |}
    """
  }

  def code(): ValidatedNel[String, String] = {
    val controllers = service.resources.map(controller)

    val str = s"""
          |object Controllers {
          |  ${controllers.mkString("\n").addMargin(2)}
          |}
        """

    Validated.validNel(str)
  }
}
