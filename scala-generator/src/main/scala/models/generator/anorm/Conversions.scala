package scala.generator.anorm

import lib.Text._
import scala.generator.{ScalaPrimitive, ScalaService}
import scala.models.TimeConfig

object Conversions {

  private val JavaPrimitiveTypes = Seq(
    ScalaPrimitive.Boolean,
    ScalaPrimitive.Double,
    ScalaPrimitive.Integer,
    ScalaPrimitive.Long,
  )

  private val JodaTimeTypes = Seq(
    ScalaPrimitive.DateIso8601Joda,
    ScalaPrimitive.DateTimeIso8601Joda,
  )

  private val JavaTimeTypes = Seq(
    ScalaPrimitive.DateIso8601Java,
    ScalaPrimitive.DateTimeIso8601Java,
  )

  private val ObjectTypes = Seq(
    ScalaPrimitive.Decimal,
    ScalaPrimitive.ObjectAsPlay,
    ScalaPrimitive.JsonValueAsPlay,
    ScalaPrimitive.String,
    ScalaPrimitive.Uuid,
  )

  private val Header = """
package %s {
%s

  /**
    * Conversions to collections of objects using JSON.
    */
  object Util {

    def parser[T](
      f: play.api.libs.json.JsValue => T
    ) = anorm.Column.nonNull { (value, meta) =>
      val MetaDataItem(columnName, nullable, clazz) = meta
      value match {
        case json: org.postgresql.util.PGobject => parseJson(f, columnName.qualified, json.getValue)
        case json: java.lang.String => parseJson(f, columnName.qualified, json)
        case _=> {
          Left(
            TypeDoesNotMatch(
              s"Column[${columnName.qualified}] error converting $value to Json. Expected instance of type[org.postgresql.util.PGobject] and not[${value.asInstanceOf[AnyRef].getClass}]"
            )
          )
        }


      }
    }

    private[this] def parseJson[T](f: play.api.libs.json.JsValue => T, columnName: String, value: String) = {
      Try {
        f(
          play.api.libs.json.Json.parse(value)
        )
      } match {
        case Success(result) => Right(result)
        case Failure(ex) => Left(
          TypeDoesNotMatch(
            s"Column[$columnName] error parsing json $value: $ex"
          )
        )
      }
    }

  }
""".trim

  def code(
    ssd: ScalaService,
    attributes: ParserGeneratorPlayVersionSpecificAttributes
  ): String = {

    val coreTypes = ssd.config.timeLib match {
      case TimeConfig.JavaTime => JavaPrimitiveTypes ++ JavaTimeTypes ++ ObjectTypes
      case TimeConfig.JodaTime => JavaPrimitiveTypes ++ JodaTimeTypes ++ ObjectTypes
    }

    Seq(
      Header.format(ssd.namespaces.anormConversions, attributes.imports.map(i => s"\n  import $i").mkString),
      Seq(
        Some("object Types {"),
        buildCollectionConversions(ssd).map(_.indent(2)),
        Some("}")
      ).flatten.mkString("\n").indent(2),
      Seq(
        "object Standard {",
        standard(coreTypes).indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  private def standard(
    types: Seq[ScalaPrimitive],
  ): String = {
    (
      Seq(
        Seq(
          s"implicit val columnToJsObject: Column[play.api.libs.json.JsObject] = Util.parser { _.as[play.api.libs.json.JsObject] }"
        )
      ) ++ types.map { t =>
        Seq(
          s"implicit val columnToSeq${t.shortName}: Column[Seq[${t.fullName}]] = Util.parser { _.as[Seq[${t.fullName}]] }",
          s"implicit val columnToMap${t.shortName}: Column[Map[String, ${t.fullName}]] = Util.parser { _.as[Map[String, ${t.fullName}]] }"
        )
      }
    ).flatten.mkString("\n")
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
                s"implicit val columnToSeq${safe}: Column[Seq[_root_.${name.qualifiedName}]] = Util.parser { _.as[Seq[_root_.${name.qualifiedName}]] }",
                s"implicit val columnToMap${safe}: Column[Map[String, _root_.${name.qualifiedName}]] = Util.parser { _.as[Map[String, _root_.${name.qualifiedName}]] }"
              ).mkString("\n")
            }.mkString("\n")
          ).mkString("\n")
        )
      }
    }
  }
}
