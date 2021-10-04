package scala.models

import io.apibuilder.generator.v0.models.Attribute

import scala.generator.ScalaPrimitive

sealed trait TimeConfig {
  def dateType: DateTypeConfig
  def dateTimeType: DateTimeTypeConfig
}
object TimeConfig {
  case object JodaTime extends TimeConfig {
    override val dateType: DateTypeConfig = DateTypeConfig.JodaLocalDate
    override val dateTimeType: DateTimeTypeConfig = DateTimeTypeConfig.JodaDateTime
  }
  case object JavaTime extends TimeConfig {
    override val dateType: DateTypeConfig = DateTypeConfig.JavaLocalDate
    override val dateTimeType: DateTimeTypeConfig = DateTimeTypeConfig.JavaInstant
  }

  private val Key = "scala_generator.time_library"
  def apply(attributes: Seq[Attribute]): Option[TimeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase.trim) match {
      case Some("joda") => Some(JodaTime)
      case Some("java") => Some(JavaTime)
      case _ => None
    }
  }
}

sealed trait DateTypeConfig {
  def dataType: ScalaPrimitive.DateIso8601
}
object DateTypeConfig {
  case object JodaLocalDate extends DateTypeConfig {
    override val dataType: ScalaPrimitive.DateIso8601 = ScalaPrimitive.DateIso8601Joda
  }
  case object JavaLocalDate extends DateTypeConfig {
    override val dataType: ScalaPrimitive.DateIso8601 = ScalaPrimitive.DateIso8601Java
  }

  private val Key = "scala_generator.date.type"
  def apply(attributes: Seq[Attribute]): Option[DateTypeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase.trim) match {
      case Some("joda.localdate") => Some(JodaLocalDate)
      case Some("java.localdate") => Some(JavaLocalDate)
      case _ => None
    }
  }
}

sealed trait DateTimeTypeConfig {
  def dataType: ScalaPrimitive.DateTimeIso8601
}
object DateTimeTypeConfig {
  case object JodaDateTime extends DateTimeTypeConfig {
    override val dataType: ScalaPrimitive.DateTimeIso8601 = ScalaPrimitive.DateTimeIso8601Joda
  }
  case object JavaInstant extends DateTimeTypeConfig {
    override val dataType: ScalaPrimitive.DateTimeIso8601 = ScalaPrimitive.DateTimeIso8601JavaInstant
  }
  case object JavaOffsetDateTime extends DateTimeTypeConfig {
    override val dataType: ScalaPrimitive.DateTimeIso8601 = ScalaPrimitive.DateTimeIso8601JavaOffsetDateTime
  }

  private val Key = "scala_generator.date_time.type"
  def apply(attributes: Seq[Attribute]): Option[DateTimeTypeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase.trim) match {
      case Some("joda.datetime") => Some(JodaDateTime)
      case Some("java.instant") => Some(JavaInstant)
      case Some("java.offsetdatetime") => Some(JavaOffsetDateTime)
      case _ => None
    }
  }
}

sealed trait JsonConfig {
  def jsonValueType: ScalaPrimitive.JsonValue
  def jsonObjectType: ScalaPrimitive.JsonObject
}
object JsonConfig {
  case object PlayJson extends JsonConfig {
    override val jsonValueType: ScalaPrimitive.JsonValue = ScalaPrimitive.JsonValueAsPlay
    override val jsonObjectType: ScalaPrimitive.JsonObject = ScalaPrimitive.ObjectAsPlay
  }
  case object CirceJson extends JsonConfig {
    override val jsonValueType: ScalaPrimitive.JsonValue = ScalaPrimitive.JsonValueAsCirce
    override val jsonObjectType: ScalaPrimitive.JsonObject = ScalaPrimitive.ObjectAsCirce
  }

  private val Key = "scala_generator.json_library"
  def apply(attributes: Seq[Attribute]): Option[JsonConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase.trim) match {
      // Don't allow json library to be configured via attributes for now as the generators currently don't support it yet
      // case Some("play") => Some(PlayJson)
      // case Some("circe") => Some(CirceJson)
      case _ => None
    }
  }
}

sealed trait ResponseConfig
object ResponseConfig {
  case object Envelope extends ResponseConfig { override def toString = "envelope" }
  case object Standard extends ResponseConfig { override def toString = "standard" }

  private val Key = "response"

  def apply(attributes: Seq[Attribute]): ResponseConfig = {
    attributes.find(_.name == Key).map(_.value.toLowerCase.trim) match {
      case Some(value) if value == Envelope.toString => ResponseConfig.Envelope
      case _ => ResponseConfig.Standard
    }
  }

}

case class Attributes(
  dateType: DateTypeConfig,
  dateTimeType: DateTimeTypeConfig,
  jsonLib: JsonConfig,
  response: ResponseConfig,
) {

  def withAttributes(attributes: Seq[Attribute]): Attributes = {
    val timeConfig = TimeConfig(attributes)
    val responseConfig = ResponseConfig(attributes)
    Attributes(
      dateType = DateTypeConfig(attributes).orElse(timeConfig.map(_.dateType)).getOrElse(dateType),
      dateTimeType = DateTimeTypeConfig(attributes).orElse(timeConfig.map(_.dateTimeType)).getOrElse(dateTimeType),
      jsonLib = JsonConfig(attributes).getOrElse(jsonLib),
      response = responseConfig,
    )
  }
}

object Attributes {
  val PlayDefaultConfig: Attributes = Attributes(DateTypeConfig.JodaLocalDate, DateTimeTypeConfig.JodaDateTime, JsonConfig.PlayJson, ResponseConfig.Standard)
  val PlayGen2DefaultConfig: Attributes = Attributes(DateTypeConfig.JavaLocalDate, DateTimeTypeConfig.JavaOffsetDateTime, JsonConfig.PlayJson, ResponseConfig.Standard)
  val Http4sDefaultConfig: Attributes = Attributes(DateTypeConfig.JavaLocalDate, DateTimeTypeConfig.JavaInstant, JsonConfig.CirceJson, ResponseConfig.Standard)
}
