package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator._
import scala.models.play.Helpers._

class BodyParsers(service: ScalaService) extends Component {

  def bodyParser(name: String, `type`: String): String = s"""
    |def bodyParser${name}(parser: BodyParser[JsValue])(implicit ec: ExecutionContext) =
    |  genericBodyParser[${`type`}](parser)
  """

  def bodyParser(enum: ScalaEnum): String = bodyParser(enum.name, enum.qualifiedName)
  def bodyParser(model: ScalaModel): String = bodyParser(model.name, model.qualifiedName)
  def bodyParser(union: ScalaUnion): String = bodyParser(union.name, union.qualifiedName)

  def genericBodyParser(packageName: String): String = s"""
    |private[${packageName}] def genericBodyParser[A : Reads](parser: BodyParser[JsValue])(implicit ec: ExecutionContext) =
    |  parser.validate(_.validate[A].asEither.left.map(e => BadRequest(JsError.toJson(e))))
  """

  def code(): ValidatedNel[String, String] = {
    val bodyParsers =
      service.enums.map(bodyParser) ++
      service.models.map(bodyParser) ++
      service.unions.map(bodyParser)

    val code = s"""
      |package object bodyparsers {
      |  import play.api.mvc.BodyParser
      |  import play.api.mvc.Results.BadRequest
      |  import play.api.libs.json.{JsError, JsValue, Reads}
      |  import scala.concurrent.ExecutionContext
      |
      |  import ${service.namespaces.base}.json._
      |
      |  ${genericBodyParser(service.namespaces.last).addMargin(4)}
      |  ${bodyParsers.mkString("\n").addMargin(4)}
      |}
    """

    Validated.validNel(code)
  }
}
