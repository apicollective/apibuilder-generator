package scala.models

import lib.Text._

import scala.generator.{ScalaEnumValue, ScalaService, ScalaUtil}

case class Play2Bindables(ssd: ScalaService) {

  def build(): String = {

    Seq(
      "object Bindables {",
      Seq(
        "import play.api.mvc.{PathBindable, QueryStringBindable}",
        buildImports(),
        buildObjectCore(),
        buildObjectModels().getOrElse(""),
        apibuilderHelpers()
      ).filter(_.trim.nonEmpty).mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  private def buildObjectCore(): String = {
    ssd.config.timeLib match {
      case TimeConfig.JodaTime => buildObjectCoreJoda()
      case TimeConfig.JavaTime => buildObjectCoreJava()
    }
  }

  private def buildObjectCoreJoda(): String = {
    """
object Core {
  implicit def pathBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.org.joda.time.DateTime] = ApibuilderPathBindable(ApibuilderTypes.dateTimeIso8601)
  implicit def queryStringBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.org.joda.time.DateTime] = ApibuilderQueryStringBindable(ApibuilderTypes.dateTimeIso8601)

  implicit def pathBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.org.joda.time.LocalDate] = ApibuilderPathBindable(ApibuilderTypes.dateIso8601)
  implicit def queryStringBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.org.joda.time.LocalDate] = ApibuilderQueryStringBindable(ApibuilderTypes.dateIso8601)
}
""".trim
  }

  private def buildObjectCoreJava(): String = {
    """
object Core {
  implicit def pathBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.java.time.Instant] = ApibuilderPathBindable(ApibuilderTypes.dateTimeIso8601)
  implicit def queryStringBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.java.time.Instant] = ApibuilderQueryStringBindable(ApibuilderTypes.dateTimeIso8601)

  implicit def pathBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.java.time.LocalDate] = ApibuilderPathBindable(ApibuilderTypes.dateIso8601)
  implicit def queryStringBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.java.time.LocalDate] = ApibuilderQueryStringBindable(ApibuilderTypes.dateIso8601)
}
""".trim
  }

  private def buildObjectModels(): Option[String] = {
    if (ssd.enums.isEmpty) {
      None
    } else {
      Some(
        Seq(
          s"object Models {",
          Seq(
            ssd.namespaces.importStatements(ssd.service).sorted.mkString("\n"),
            ssd.enums.map { e => buildImplicit(e.name) }.mkString("\n\n")
          ).filter(_.nonEmpty).mkString("\n\n").indent(2),
          "}"
        ).mkString("\n")
      )
    }
  }

  private[this] def buildImports(): String = {
    Seq(
      "// import models directly for backwards compatibility with prior versions of the generator",
      "import Core._",
      if (ssd.enums.isEmpty) {
        ""
      } else {
        "import Models._"
      }
    ).filter(_.nonEmpty).mkString("\n")
  }

  private def apibuilderTypesJoda = {
    """object ApibuilderTypes {
      |  import org.joda.time.{format, DateTime, LocalDate}
      |
      |  val dateTimeIso8601: ApibuilderTypeConverter[DateTime] = new ApibuilderTypeConverter[DateTime] {
      |    override def convert(value: String): DateTime = format.ISODateTimeFormat.dateTimeParser.parseDateTime(value)
      |    override def convert(value: DateTime): String = format.ISODateTimeFormat.dateTime.print(value)
      |    override def example: DateTime = DateTime.now
      |  }
      |
      |  val dateIso8601: ApibuilderTypeConverter[LocalDate] = new ApibuilderTypeConverter[LocalDate] {
      |    override def convert(value: String): LocalDate = format.ISODateTimeFormat.yearMonthDay.parseLocalDate(value)
      |    override def convert(value: LocalDate): String = value.toString
      |    override def example: LocalDate = LocalDate.now
      |  }
      |
      |}""".stripMargin
  }

  private def apibuilderTypesJava = {
    """object ApibuilderTypes {
      |  import java.time.{Instant, LocalDate}
      |
      |  val dateTimeIso8601: ApibuilderTypeConverter[Instant] = new ApibuilderTypeConverter[Instant] {
      |    override def convert(value: String): Instant = Instant.parse(value)
      |    override def convert(value: Instant): String = value.toString
      |    override def example: Instant = Instant.now
      |  }
      |
      |  val dateIso8601: ApibuilderTypeConverter[LocalDate] = new ApibuilderTypeConverter[LocalDate] {
      |    override def convert(value: String): LocalDate = LocalDate.parse(value)
      |    override def convert(value: LocalDate): String = value.toString
      |    override def example: LocalDate = LocalDate.now
      |  }
      |
      |}""".stripMargin
  }

  private def apibuilderHelpers(): String = {
    val apibuilderTypes = ssd.config.timeLib match {
      case TimeConfig.JodaTime => apibuilderTypesJoda
      case TimeConfig.JavaTime => apibuilderTypesJava
    }

    s"""
trait ApibuilderTypeConverter[T] {

  def convert(value: String): T

  def convert(value: T): String

  def example: T

  def validValues: Seq[T] = Nil

  def errorMessage(key: String, value: String, ex: java.lang.Exception): String = {
    val base = s"Invalid value '$$value' for parameter '$$key'. "
    validValues.toList match {
      case Nil => base + "Ex: " + convert(example)
      case values => base + ". Valid values are: " + values.mkString("'", "', '", "'")
    }
  }
}

$apibuilderTypes

final case class ApibuilderQueryStringBindable[T](
  converters: ApibuilderTypeConverter[T]
) extends QueryStringBindable[T] {

  override def bind(key: String, params: Map[String, Seq[String]]): _root_.scala.Option[_root_.scala.Either[String, T]] = {
    params.getOrElse(key, Nil).headOption.map { v =>
      try {
        Right(
          converters.convert(v)
        )
      } catch {
        case ex: java.lang.Exception => Left(
          converters.errorMessage(key, v, ex)
        )
      }
    }
  }

  override def unbind(key: String, value: T): String = {
    s"$$key=$${converters.convert(value)}"
  }
}

final case class ApibuilderPathBindable[T](
  converters: ApibuilderTypeConverter[T]
) extends PathBindable[T] {

  override def bind(key: String, value: String): _root_.scala.Either[String, T] = {
    try {
      Right(
        converters.convert(value)
      )
    } catch {
      case ex: java.lang.Exception => Left(
        converters.errorMessage(key, value, ex)
      )
    }
  }

  override def unbind(key: String, value: T): String = {
    converters.convert(value)
  }
}
""".trim
  }

  private[models] def buildImplicit(
    enumName: String
  ): String = {
    val fullyQualifiedName = ssd.enumClassName(enumName)
    val converter = ScalaUtil.toVariable(enumName) + "Converter"
    Seq(
      s"val $converter: ApibuilderTypeConverter[$fullyQualifiedName] = ${buildEnumConverter(enumName)}",
      s"implicit def pathBindable$enumName(implicit stringBinder: QueryStringBindable[String]): PathBindable[$fullyQualifiedName] = ApibuilderPathBindable($converter)",
      s"implicit def queryStringBindable$enumName(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[$fullyQualifiedName] = ApibuilderQueryStringBindable($converter)"
    ).mkString("\n")
  }

  private[this] def buildEnumConverter(enumName: String): String = {
    val fullyQualifiedName = ssd.enumClassName(enumName)
    val example = exampleEnumValue(enumName)

    Seq(
      s"new ApibuilderTypeConverter[$fullyQualifiedName] {",
      s"  override def convert(value: String): $fullyQualifiedName = $fullyQualifiedName(value)",
      s"  override def convert(value: $fullyQualifiedName): String = value.toString",
      s"  override def example: $fullyQualifiedName = $fullyQualifiedName.${example.name}",
      s"  override def validValues: Seq[$fullyQualifiedName] = $fullyQualifiedName.all",
      s"}"
    ).mkString("\n")
  }

  private[this] def exampleEnumValue(enumName: String): ScalaEnumValue = {
    val fullyQualifiedName = ssd.enumClassName(enumName)
    val enum = ssd.enums.find(_.qualifiedName == fullyQualifiedName).getOrElse {
      sys.error(
        s"Failed to find enum[$fullyQualifiedName] in service[${ssd.service.name}]." +
          s"Available enums: ${ssd.enums.map(_.qualifiedName).mkString(", ")}"
      )
    }

    enum.values.headOption.getOrElse {
      sys.error(s"Enum[$fullyQualifiedName] does not have any values")
    }
  }
}
