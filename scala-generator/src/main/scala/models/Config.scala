package scala.models

import io.apibuilder.generator.v0.models.Attribute

import scala.generator.ScalaPrimitive

sealed trait TimeConfig {
  def dateType: DateTypeConfig
  def dateTimeType: DateTimeTypeConfig
}
object TimeConfig {
  case object JodaTime extends TimeConfig {
    override val dateType = DateTypeConfig.JodaLocalDate
    override val dateTimeType = DateTimeTypeConfig.JodaDateTime
  }
  case object JavaTime extends TimeConfig {
    override val dateType = DateTypeConfig.JavaLocalDate
    override val dateTimeType = DateTimeTypeConfig.JavaInstant
  }

  private val Key = "scala_generator.time_library"
  def apply(attributes: Seq[Attribute]): Option[TimeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase) match {
      case Some("joda") => Some(JodaTime)
      case Some("java") => Some(JavaTime)
      case _ => None
    }
  }
}

sealed trait DateTypeConfig {
  def dataType: ScalaPrimitive
}
object DateTypeConfig {
  case object JodaLocalDate extends DateTypeConfig {
    override val dataType = ScalaPrimitive.DateIso8601Joda
  }
  case object JavaLocalDate extends DateTypeConfig {
    override val dataType = ScalaPrimitive.DateIso8601Java
  }

  private val Key = "scala_generator.date_time.type"
  def apply(attributes: Seq[Attribute]): Option[DateTypeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase) match {
      case Some("joda.localdate") => Some(JodaLocalDate)
      case Some("java.localdate") => Some(JavaLocalDate)
      case _ => None
    }
  }
}

sealed trait DateTimeTypeConfig {
  def dataType: ScalaPrimitive
}
object DateTimeTypeConfig {
  case object JodaDateTime extends DateTimeTypeConfig {
    override val dataType = ScalaPrimitive.DateTimeIso8601Joda
  }
  case object JavaInstant extends DateTimeTypeConfig {
    override val dataType = ScalaPrimitive.DateTimeIso8601JavaInstant
  }
  case object JavaOffsetDateTime extends DateTimeTypeConfig {
    override val dataType = ScalaPrimitive.DateTimeIso8601JavaOffsetDateTime
  }

  private val Key = "scala_generator.date_time.type"
  def apply(attributes: Seq[Attribute]): Option[DateTimeTypeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase) match {
      case Some("joda.datetime") => Some(JodaDateTime)
      case Some("java.instant") => Some(JavaInstant)
      case Some("java.offsetdatetime") => Some(JavaOffsetDateTime)
      case _ => None
    }
  }
}

sealed trait JsonConfig
object JsonConfig {
  case object PlayJson extends JsonConfig
  case object CirceJson extends JsonConfig

  private val Key = "scala_generator.json_library"
  def apply(attributes: Seq[Attribute]): Option[JsonConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase) match {
      // Don't allow json library to be configured via attributes for now as the generators currently don't support it yet
      // case Some("play") => Some(PlayJson)
      // case Some("circe") => Some(CirceJson)
      case _ => None
    }
  }
}

case class Config(dateType: DateTypeConfig, dateTimeType: DateTimeTypeConfig, jsonLib: JsonConfig)

object Config {
  val PlayDefaultConfig = Config(DateTypeConfig.JodaLocalDate, DateTimeTypeConfig.JodaDateTime, JsonConfig.PlayJson)
  val PlayGen2DefaultConfig = Config(DateTypeConfig.JavaLocalDate, DateTimeTypeConfig.JavaInstant, JsonConfig.PlayJson)
  val Http4sDefaultConfig = Config(DateTypeConfig.JavaLocalDate, DateTimeTypeConfig.JavaInstant, JsonConfig.CirceJson)

  def apply(attributes: Seq[Attribute], defaultConfig: Config): Config = {
    val timeCfg = TimeConfig(attributes)
    Config(
      dateType = DateTypeConfig(attributes).orElse(timeCfg.map(_.dateType)).getOrElse(defaultConfig.dateType),
      dateTimeType = DateTimeTypeConfig(attributes).orElse(timeCfg.map(_.dateTimeType)).getOrElse(defaultConfig.dateTimeType),
      jsonLib = JsonConfig(attributes).getOrElse(defaultConfig.jsonLib),
    )
  }
}
