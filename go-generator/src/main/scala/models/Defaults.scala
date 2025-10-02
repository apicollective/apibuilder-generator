package go.models

import io.apibuilder.spec.v0.models.Service
import lib.Datatype
import play.api.libs.json.*

object Defaults {

  /**
    * Returns a default for a field if
    * - a default exists
    * - the default can be parsed into a Golang type
    *
    * The string returned should be ready to include on the right side of an assignment. Examples are
    * true
    * "hello world"
    * 7.85
    * string[]
    *
    */
  def mkGolangDefault(default: Option[String], dt: Datatype, svc: Service,  importBuilder: ImportBuilder): Option[String] = {
    default match {
      case Some(defaultText) => {
        dt match {
          case Datatype.Primitive.Boolean => Some(wrapInQuotes(defaultText))
          case Datatype.Primitive.Decimal => Some(toBigDecimal(Json.parse(defaultText)).toString)
          case Datatype.Primitive.Integer => Some(toBigDecimal(Json.parse(defaultText)).toInt.toString)
          case Datatype.Primitive.Double => Some(toBigDecimal(Json.parse(defaultText)).toDouble.toString)
          case Datatype.Primitive.Long => Some(toBigDecimal(Json.parse(defaultText)).toLong.toString)
          case Datatype.Primitive.String => Some(wrapInQuotes(defaultText))
          case Datatype.Primitive.Uuid => Some(wrapInQuotes(defaultText))
          case Datatype.Primitive.DateTimeIso8601 => Some(wrapInQuotes(defaultText))
          case Datatype.Primitive.DateIso8601 => Some(wrapInQuotes(defaultText))
          case Datatype.UserDefined.Enum(enumName) => {
              svc.enums.find(_.name == enumName).flatMap(e=> e.values.find(_.name == defaultText).map(v=> importBuilder.publicName(enumName) + importBuilder.publicName(v.name)))
          }
          case Datatype.Container.List(t) => {
            val arr = Json.parse(defaultText).as[JsArray]
            val seq = arr.value.map { value =>
              mkGolangDefault(Some(value.toString), t, svc, importBuilder)
            }
            if (seq.isEmpty) {
              None
            } else {
              Some(s"[]string{${seq.collect{case Some(x)=>x}.mkString(",")}}")
            }
          }
          case _ => None
        }
      }
      case None => None
    }
  }

  private def toBigDecimal(json: JsValue): BigDecimal = {
    json match {
      case v: JsString => {
        if (v.value.matches("0+")) {
          BigDecimal("0")
        } else {
          BigDecimal(json.toString)
        }
      }
      case v: JsNumber => v.value
      case _ => BigDecimal(json.toString)
    }
  }

  private def wrapInQuotes(value: String): String = {
    val inQuotesAlready = "\".*\"".r
    value match {
      case inQuotesAlready() => value
      case _ => s""""$value""""
    }
  }
}
