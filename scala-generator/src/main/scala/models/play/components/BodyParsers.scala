package scala.models.play.components

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import scala.generator._

case object GenericBodyParserDef {
  def name = "genericBodyParser"
  def syntax: String = s"""
    private def ${name}[A](parser: _root_.play.api.mvc.BodyParser[_root_.play.api.libs.json.JsValue])(implicit ec: _root_.scala.concurrent.ExecutionContext, rds: _root_.play.api.libs.json.Reads[A]): _root_.play.api.mvc.BodyParser[A] =
      parser.validate(_.validate[A].asEither.left.map(e => _root_.play.api.mvc.Results.BadRequest(_root_.play.api.libs.json.JsError.toJson(e))))
  """
}

case class BodyParserDef(suffix: String, tpe: String) {
  def syntax: String = {
    val name = s"bodyParser${suffix}"
    s"""
      def ${name}(parser: _root_.play.api.mvc.BodyParser[_root_.play.api.libs.json.JsValue])(implicit ec: _root_.scala.concurrent.ExecutionContext, rds: _root_.play.api.libs.json.Reads[${tpe}]): _root_.play.api.mvc.BodyParser[${tpe}] =
        ${GenericBodyParserDef.name}[${tpe}](parser)
    """
  }
}

case class BodyParsersObject(packageObject: String, bodyParsers: List[BodyParserDef]) {
  def syntax: String = s"""
    package ${packageObject.split('.').dropRight(1).mkString(".")}

    package object ${packageObject.split('.').last} {
      ${GenericBodyParserDef.syntax}
      ${bodyParsers.map(_.syntax).mkString("\n")}
    }
  """
}

object BodyParsers extends Component {
  def bodyParsers(service: ScalaService): List[BodyParserDef] = (
      service.enums.map { e => (e.name, e.qualifiedName) } ++
      service.models.map { m => (m.name, m.qualifiedName) } ++
      service.unions.map { u => (u.name, u.qualifiedName) }
    )
    .toList
    .map((BodyParserDef.apply _).tupled)

  def bodyParsersObject(service: ScalaService): BodyParsersObject =
    BodyParsersObject(service.namespaces.bodyParsers, bodyParsers(service))

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val syntax = bodyParsersObject(service).syntax
    Validated.validNel(syntax)
  }
}
