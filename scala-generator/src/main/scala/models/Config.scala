package scala.models

import io.apibuilder.generator.v0.models.Attribute

sealed trait TimeConfig
object TimeConfig {
  case object JodaTime extends TimeConfig
  case object JavaTime extends TimeConfig

  private val Key = "scala_generator.time_library"
  def apply(attributes: Seq[Attribute]): Option[TimeConfig] = {
    attributes.find(_.name == Key).map(_.value.toLowerCase) match {
      case Some("joda") => Some(JodaTime)
      case Some("java") => Some(JavaTime)
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

case class Config(timeLib: TimeConfig, jsonLib: JsonConfig)

object Config {
  val PlayDefaultConfig = Config(TimeConfig.JodaTime, JsonConfig.PlayJson)
  val PlayGen2DefaultConfig = Config(TimeConfig.JavaTime, JsonConfig.PlayJson)
  val Http4sDefaultConfig = Config(TimeConfig.JavaTime, JsonConfig.CirceJson)

  def apply(attributes: Seq[Attribute], defaultConfig: Config): Config = {
    Config(
      timeLib = TimeConfig(attributes).getOrElse(defaultConfig.timeLib),
      jsonLib = JsonConfig(attributes).getOrElse(defaultConfig.jsonLib),
    )
  }
}
