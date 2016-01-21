package scala.generator.anorm

import lib.Text._
import scala.generator.{Namespaces, ScalaPrimitive, ScalaService}

object Conversions {

  private val Types = Seq(
    ScalaPrimitive.Boolean,
    ScalaPrimitive.Double,
    ScalaPrimitive.Integer,
    ScalaPrimitive.Long,
    ScalaPrimitive.DateIso8601,
    ScalaPrimitive.DateTimeIso8601,
    ScalaPrimitive.Decimal,
    ScalaPrimitive.Object,
    ScalaPrimitive.String,
    ScalaPrimitive.Uuid
  )

  private val Header = """
package %s {

  import anorm.{Column, MetaDataItem, TypeDoesNotMatch}
  import play.api.libs.json.{JsArray, JsObject, JsValue}
  import scala.util.{Failure, Success, Try}

  /**
    * Conversions to collections of objects using JSON.
    */
  object Json {

    def parser[T](
      f: play.api.libs.json.JsValue => T
    ) = anorm.Column.nonNull1 { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case json: org.postgresql.util.PGobject => {
          Try {
            f(
              play.api.libs.json.Json.parse(
                json.getValue
              )
            )
          } match {
            case Success(result) => Right(result)
            case Failure(ex) => Left(
              TypeDoesNotMatch(
                s"Column[$qualified] error parsing json $value: $ex"
              )
            )
          }
        }
        case _=> {
          Left(
            TypeDoesNotMatch(
              s"Column[$qualified] error converting $value: ${value.asInstanceOf[AnyRef].getClass} to Json"
            )
          )
        }


      }
    }
""".trim

  private val Footer = "  }\n\n}"

  def code(
    ssd: ScalaService
  ): String = {

    Seq(
      Some(Header.format(ssd.namespaces.anormConversions)),
      Some(s"    implicit val columnToJsObject: Column[play.api.libs.json.JsObject] = parser { _.as[play.api.libs.json.JsObject] }"),
      Some(
        Types.map { t =>
          Seq(
            s"implicit val columnToSeq${t.shortName}: Column[Seq[${t.fullName}]] = parser { _.as[Seq[${t.fullName}]] }",
            s"implicit val columnToMap${t.shortName}: Column[Map[String, ${t.fullName}]] = parser { _.as[Map[String, ${t.fullName}]] }"
          ).mkString("\n").indent(2)
        }.mkString("\n").indent(2)
      ),
      buildCollectionConversions(ssd).map(_.indent(4)),
      Some(Footer)
    ).flatten.mkString("\n\n")

  }


  private[this] case class Name(shortName: String, qualifiedName: String)

  private[this] def buildCollectionConversions(ssd: ScalaService): Option[String] = {
    (
      ssd.enums.map(e => Name(e.name, e.qualifiedName)) ++
        ssd.models.map(m => Name(m.name, m.qualifiedName)) ++
        ssd.unions.map(u => Name(u.name, u.qualifiedName))
    ) match {
      case Nil => None
      case names => {
        Some(
          Seq(
            s"import ${ssd.namespaces.json}._",
            names.map { name =>
              val safe = ssd.name + name.shortName
              Seq(
                s"implicit val columnToSeq${safe}: Column[Seq[_root_.${name.qualifiedName}]] = parser { _.as[Seq[_root_.${name.qualifiedName}]] }",
                s"implicit val columnToMap${safe}: Column[Map[String, _root_.${name.qualifiedName}]] = parser { _.as[Map[String, _root_.${name.qualifiedName}]] }"
              ).mkString("\n")
            }.mkString("\n")
          ).mkString("\n")
        )
      }
    }
  }
}
