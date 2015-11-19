package scala.generator.anorm

import lib.Text._
import scala.generator.ScalaPrimitive
import scala.generator.Namespaces

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

  def code(namespaces: Namespaces): String = {

    Seq(
      Header.format(namespaces.anormConversions),
      Types.map { t =>
        Seq(
          s"implicit val columnToSeq${t.shortName}: Column[Seq[${t.fullName}]] = parser { _.as[Seq[${t.fullName}]] }",
          s"implicit val columnToMap${t.shortName}: Column[Map[String, ${t.fullName}]] = parser { _.as[Map[String, ${t.fullName}]] }"
        ).mkString("\n").indent(2)
      }.mkString("\n").indent(2),
      Footer
    ).mkString("\n\n")

  }

}
