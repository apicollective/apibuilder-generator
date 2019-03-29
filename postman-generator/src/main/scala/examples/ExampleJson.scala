package examples

import java.util.UUID

import io.apibuilder.spec.v0.models._
import io.apibuilder.spec.v0.models.json._
import io.postman.generator.attributes.v0.models.{AttributeName, ObjectReference, ValueSubstitute}
import io.postman.generator.attributes.v0.models.json.{jsonReadsPostmanGeneratorAttributesObjectReference, jsonReadsPostmanGeneratorAttributesValueSubstitute}
import models.AttributeValueReader
import models.attributes.PostmanAttributes.ObjectReferenceExtend
import org.joda.time.{DateTime, LocalDate}
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.json._

import scala.util.Random

object ExampleJson {
  val TrueStrings = Seq("t", "true", "y", "yes", "on", "1", "trueclass")
  val FalseStrings = Seq("f", "false", "n", "no", "off", "0", "falseclass")
}

trait Selection
object Selection {
  case object RequiredFieldsOnly extends Selection
  case object All extends Selection
}

case class UnknownType(typ: String) extends Throwable

case class ExampleJson(
  service: Service,
  selection: Selection,
  randomStringGenerator: RandomStringGenerator) {

  def sample(typ: String, subTyp: Option[String] = None): Option[JsValue] = {
    try {
      subTyp match {
        case Some(subType) => makeUnion(typ, subType)
        case None => Some(
          mockValue(TextDatatype.parse(typ), None)
        )
      }
    } catch {
      case UnknownType(_) => None
      case ex: Throwable => throw new RuntimeException(ex)
    }
  }

  private[this] def makeEnum(enum: Enum, parentUnion: Option[(Union, UnionType)], outerField: Option[Field]): JsValue = {
    def randomValue: JsValue = JsString(
      Random.shuffle(enum.values)
        .headOption.map(ev => ev.value.getOrElse(ev.name)).getOrElse("undefined")
    )
    val value = outerField
      .flatMap(field => field.example orElse field.default)
      .map(JsString)
      .getOrElse(randomValue)

    parentUnion.fold(value) { case (union, unionType) =>
      // strip any namespace prefix from model name
      val name = enum.name.reverse.takeWhile(_ != '.').reverse
      val discrVal = unionType.discriminatorValue.getOrElse(name)
      union.discriminator.fold {
        Json.obj(discrVal -> value)
      }{ discriminator =>
        Json.obj(
          discriminator -> JsString(discrVal),
          "value" -> value
        )
      }
    }

  }

  private[this] def makeModel(model: Model, parentUnion: Option[(Union, UnionType)]): JsValue = {
    val value = JsObject(
      Map(
        model.fields.
          filter { f => selection == Selection.All || f.required }.
          map { field =>

            val valueSubstituteAttrOpt = AttributeValueReader.findAndReadFirst[ValueSubstitute](field.attributes, AttributeName.ValueSubstitute)
            val valueSubstituteOpt = valueSubstituteAttrOpt.map(_.substitute)
            val value = valueSubstituteOpt match {
              case Some(valueSubstitute) => JsString(valueSubstitute)
              case None =>
                val objRefAttrOpt = AttributeValueReader.findAndReadFirst[ObjectReference](field.attributes, AttributeName.ObjectReference)
                val postmanVariableRefOpt = objRefAttrOpt.map(_.toExtended.postmanVariableName.reference)

                postmanVariableRefOpt.map { postmanVariableRef =>
                  if (field.`type`.equalsIgnoreCase("string"))
                    JsString(postmanVariableRef)
                  else if (field.`type`.equalsIgnoreCase("[string]"))
                    Json.arr(JsString(postmanVariableRef))
                  else if (field.`type`.equalsIgnoreCase("map[string]"))
                    Json.obj(postmanVariableRef -> JsString("bar"))
                  else
                    mockValue(field)
                }.getOrElse(mockValue(field))
            }

            field.name -> value
          }: _*
      )
    )

    parentUnion.fold(value) { case (union, unionType) =>
      // strip any namespace prefix from model name
      val name = model.name.reverse.takeWhile(_ != '.').reverse
      val discrVal = unionType.discriminatorValue.getOrElse(name)
      union.discriminator.fold {
        Json.obj(discrVal -> value)
      }{ discriminator =>
        Json.obj(discriminator -> JsString(discrVal)) ++ value
      }
    }
  }

  private[this] def makeUnion(unionName: String, unionTypeName: String): Option[JsValue] = {
    val unions = for {
      union <- service.unions if union.name == unionName
      unionType <- union.types if (unionType.`type` == unionTypeName)
    } yield makeUnion(union, Some(unionType))
    unions.headOption
  }

  private[this] def makeUnion(union: Union, unionType: Option[UnionType] = None): JsValue = {
    unionType.orElse(union.types.headOption).fold {
      Json.obj(): JsValue
    } { unionType =>
      mockValue(TextDatatype.parse(unionType.`type`), Some(union -> unionType)) match {
        case js: JsBoolean => primitiveUnionWrapper(union, unionType, js)
        case js: JsNumber => primitiveUnionWrapper(union, unionType, js)
        case js: JsString => primitiveUnionWrapper(union, unionType, js)
        case JsNull => primitiveUnionWrapper(union, unionType, JsNull)
        case other => other
      }
    }
  }

  // primitives in a union type are wrapped in a 'value' field
  private[this] def primitiveUnionWrapper(union: Union, unionType: UnionType, js: JsValue): JsValue = {
    val discrVal = unionType.discriminatorValue.getOrElse(unionType.`type`)
    union.discriminator.fold {
      Json.obj(discrVal -> Json.obj("value" -> js))
    } { discriminator =>
      Json.obj(
        discriminator -> JsString(discrVal),
        "value" -> js
      )
    }
  }

  private[this] def mockValue(types: Seq[TextDatatype], parentUnion: Option[(Union, UnionType)]): JsValue = {
    types.toList match {
      case Nil => JsNull
      case TextDatatype.Singleton(one) :: Nil => singleton(one, parentUnion)
      case TextDatatype.Singleton(one) :: _ => sys.error("Singleton must be leaf")
      case TextDatatype.List :: rest => Json.toJson(Seq(mockValue(rest, None)))
      case TextDatatype.Map :: rest => Json.obj("foo" -> mockValue(rest, None))
    }
  }

  private[this] def singleton(typ: String, parentUnion: Option[(Union, UnionType)]): JsValue = {
    Primitives(typ) match {
      case None => {
        service.enums.find(_.name == typ) match {
          case Some(e) => makeEnum(e, parentUnion, None)
          case None => {
            service.models.find(_.name == typ) match {
              case Some(m) => makeModel(m, parentUnion)

              case None => {
                service.unions.find(_.name == typ) match {
                  case Some(u) => makeUnion(u)
                  case None => throw new UnknownType(typ)
                }
              }
            }
          }
        }
      }

      case Some(p) => mockPrimitive(p)
    }
  }

  private[this] def mockValue(field: Field): JsValue = {
    val types = TextDatatype.parse(field.`type`)
    types.toList match {
      case Nil => JsNull
      case TextDatatype.Singleton(one) :: Nil => singleton(field)
      case TextDatatype.Singleton(_) :: _ => sys.error("Singleton must be leaf")
      case TextDatatype.List :: rest => {
        field.default match {
          case None => {
            Json.toJson(Seq(mockValue(rest, None)))
          }
          case Some(default) => {
            try {
              Json.parse(default).as[JsArray]
            } catch {
              case _: Throwable => Json.toJson(Seq(mockValue(rest, None)))
            }
          }
        }
      }
      case TextDatatype.Map :: rest => {
        field.default match {
          case None => {
            Json.obj("foo" -> mockValue(rest, None))
          }
          case Some(default) => {
            try {
              Json.parse(default).as[JsObject]
            } catch {
              case _: Throwable => Json.obj("foo" -> mockValue(rest, None))
            }
          }
        }
      }
    }
  }

  private[this] def singleton(field: Field): JsValue = {
    Primitives(field.`type`) match {
      case None => {
        service.enums.find(_.name == field.`type`) match {
          case Some(e) => makeEnum(e, None, Some(field))
          case None => {
            service.models.find(_.name == field.`type`) match {
              case Some(m) => makeModel(m, None)
              case None => {
                service.unions.find(_.name == field.`type`) match {
                  case Some(u) => makeUnion(u)
                  case None => throw new UnknownType(field.`type`)
                }
              }
            }
          }
        }
      }

      case Some(p) => {
        field.default match {
          case Some(default) => primitiveExample(p, default)
          case None => {
            field.example match {
              case None => mockPrimitive(p, Some(field))
              case Some(ex) => primitiveExample(p, ex)
            }
          }
        }
      }
    }
  }

  private[this] def mockPrimitive(p: Primitives, fieldOpt: Option[Field] = None): JsValue = {
    p match {
      case Primitives.Boolean => JsBoolean(true)
      case Primitives.Double => JsNumber(1.0)
      case Primitives.Integer => JsNumber(1)
      case Primitives.Long => JsNumber(1)
      case Primitives.DateIso8601 => {
        val now = DateTime.now
        JsString(s"${now.year}-${now.monthOfYear()}-${now.dayOfMonth()}")
      }
      case Primitives.DateTimeIso8601 => JsString(ISODateTimeFormat.dateTime.print(DateTime.now))
      case Primitives.Decimal => Json.toJson(BigDecimal("1"))
      case Primitives.String => JsString(randomString(fieldOpt))
      case Primitives.Object => Json.obj("foo" -> "bar")
      case Primitives.JsonValue => JsNull
      case Primitives.Unit => JsNull
      case Primitives.Uuid => JsString(UUID.randomUUID().toString)
    }
  }

  private[this] def primitiveExample(p: Primitives, ex: String): JsValue = {
    p match {
      case Primitives.Boolean => JsBoolean(parseBoolean(ex, true))
      case Primitives.Double => JsNumber(parseDouble(ex, 1.0))
      case Primitives.Integer => JsNumber(parseInt(ex, 1))
      case Primitives.Long => JsNumber(parseInt(ex, 1))
      case Primitives.DateIso8601 => {
        val ts = parseDate(ex, LocalDate.now)
        JsString(s"${ts.year}-${ts.monthOfYear()}-${ts.dayOfMonth()}")
      }
      case Primitives.DateTimeIso8601 => {
        val ts = parseDateTime(ex, DateTime.now)
        JsString(ISODateTimeFormat.dateTime.print(ts))
      }
      case Primitives.Decimal => Json.toJson(BigDecimal(parseDouble(ex, 1)))
      case Primitives.String => JsString(ex)
      case Primitives.Object => parseObject(ex, Json.obj("foo" -> "bar"))
      case Primitives.JsonValue => parseJsonValue(ex, JsNull)
      case Primitives.Unit => JsNull
      case Primitives.Uuid => JsString(parseUUID(ex, UUID.randomUUID).toString)
    }
  }

  private[this] def parseBoolean(value: String, default: Boolean): Boolean = {
    if (ExampleJson.TrueStrings.contains(value.toLowerCase().trim)) {
      true
    } else if (ExampleJson.TrueStrings.contains(value.toLowerCase().trim)) {
      false
    } else {
      default
    }
  }

  private[this] def parseDouble(value: String, default: Double): Double = {
    try {
      value.toDouble
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def parseInt(value: String, default: Int): Int = {
    try {
      value.toInt
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def parseUUID(value: String, default: UUID): UUID = {
    try {
      UUID.fromString(value)
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def parseObject(value: String, default: JsObject): JsObject = {
    try {
      Json.parse(value).as[JsObject]
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def parseJsonValue(value: String, default: JsValue): JsValue = {
    try {
      Json.parse(value)
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def parseDate(value: String, default: LocalDate): LocalDate = {
    try {
      ISODateTimeFormat.dateTimeParser.parseLocalDate(value)
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def parseDateTime(value: String, default: DateTime): DateTime = {
    try {
      ISODateTimeFormat.dateTimeParser.parseDateTime(value)
    } catch {
      case _: Throwable => default
    }
  }

  private[this] def randomString(fieldOpt: Option[Field] = None): String = {
    val seed = {
      fieldOpt
        .map(_.name)
        .getOrElse(Random.nextString(12))
    }

    randomStringGenerator.generate(seed)
  }

}

sealed trait Primitives

object Primitives {

  case object Boolean extends Primitives { override def toString = "boolean" }
  case object Decimal extends Primitives { override def toString = "decimal" }
  case object Integer extends Primitives { override def toString = "integer" }
  case object Double extends Primitives { override def toString = "double" }
  case object Long extends Primitives { override def toString = "long" }
  case object Object extends Primitives { override def toString = "object" }
  case object JsonValue extends Primitives { override def toString = "json" }
  case object String extends Primitives { override def toString = "string" }
  case object DateIso8601 extends Primitives { override def toString = "date-iso8601" }
  case object DateTimeIso8601 extends Primitives { override def toString = "date-time-iso8601" }
  case object Uuid extends Primitives { override def toString = "uuid" }
  case object Unit extends Primitives { override def toString = "unit" }

  val All: Seq[Primitives] = Seq(Boolean, Decimal, Integer, Double, Long, Object, JsonValue, String, DateIso8601, DateTimeIso8601, Uuid, Unit)

  val ValidInPath: Seq[Primitives] = All.filter(p => p != Unit && p != Object && p != JsonValue)

  def validInUrl(name: String): Boolean = {
    Primitives(name) match {
      case None => false
      case Some(p) => ValidInPath.contains(p)
    }
  }

  private[this] val byName: Map[String, Primitives] = All.map(x => x.toString.toLowerCase -> x).toMap

  def apply(value: String): Option[Primitives] = byName.get(value.toLowerCase.trim)

}

sealed trait TextDatatype

object TextDatatype {

  case object List extends TextDatatype
  case object Map extends TextDatatype
  case class Singleton(name: String) extends TextDatatype

  private[this] val ListRx = "^\\[(.*)\\]$".r
  private[this] val MapRx = "^map\\[(.*)\\]$".r
  private[this] val MapDefaultRx = "^map$".r

  def parse(value: String): Seq[TextDatatype] = {
    value match {
      case ListRx(t) => Seq(TextDatatype.List) ++ parse(t)
      case MapRx(t) => Seq(TextDatatype.Map) ++ parse(t)
      case MapDefaultRx() => Seq(TextDatatype.Map, TextDatatype.Singleton(Primitives.String.toString))
      case _ => Seq(TextDatatype.Singleton(value))
    }
  }
}

import scala.util.Random
import java.security.SecureRandom

object TokenGenerator {

  private[this] val random = new Random(new SecureRandom())
  private[this] val Alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  def generate(n: Int = 80): String = {
    Stream.continually(random.nextInt(Alphabet.size)).map(Alphabet).take(n).mkString
  }

}

