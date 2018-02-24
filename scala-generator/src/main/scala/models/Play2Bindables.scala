package scala.models

import scala.generator.ScalaService

case class Play2Bindables(ssd: ScalaService) {

  def build(): String = {
    import lib.Text._

    Seq(
      "object Bindables {",
      "",
      "  import play.api.mvc.{PathBindable, QueryStringBindable}",
      "  import org.joda.time.{DateTime, LocalDate}",
      "  import org.joda.time.format.ISODateTimeFormat",
      "  " + ssd.namespaces.importStatements(ssd.service).mkString("\n  "),
      "",
      buildDefaults().indent(2),
      "",
      ssd.enums.map { e => buildImplicit(e.name) }.mkString("\n\n").indent(2),
      "",
      "}"
    ).mkString("\n")
  }

  private def buildDefaults(): String = {
    """
trait ApibuilderTypeConverter[T] {

  def convert(value: String): T

  def convert(value: T): String

  def example: T

  def validValues: Seq[T] = Nil

  def errorMessage(key: String, value: String, ex: java.lang.Exception): String = {
    val base = s"Invalid value '$value' for parameter '$key'. "
    validValues.toList match {
      case Nil => base + "Ex: " + convert(example)
      case values => base + ". Valid values are: " + values.mkString("'", "', '", "'")
    }
  }
}

object ApibuilderTypeConverter {

  val dateTimeIso8601: ApibuilderTypeConverter[DateTime] = new ApibuilderTypeConverter[DateTime] {
    override def convert(value: String): DateTime = ISODateTimeFormat.dateTimeParser.parseDateTime(value)
    override def convert(value: DateTime): String = ISODateTimeFormat.dateTime.print(value)
    override def example: DateTime = DateTime.now
  }

  val dateIso8601: ApibuilderTypeConverter[LocalDate] = new ApibuilderTypeConverter[LocalDate] {
    override def convert(value: String): LocalDate = ISODateTimeFormat.yearMonthDay.parseLocalDate(value)
    override def convert(value: LocalDate): String = value.toString
    override def example: LocalDate = LocalDate.now
  }

}

case class ApibuilderQueryStringBindable[T](
  converters: ApibuilderTypeConverter[T]
) extends QueryStringBindable[T] {

  override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, T]] = {
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
    converters.convert(value)
  }
}

case class ApibuilderPathBindable[T](
  converters: ApibuilderTypeConverter[T]
) extends PathBindable[T] {

  override def bind(key: String, value: String): Either[String, T] = {
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

implicit val pathBindableTypeDateTimeIso8601 = ApibuilderPathBindable(ApibuilderTypeConverter.dateTimeIso8601)
implicit val queryStringBindableDateTimeIso8601 = ApibuilderQueryStringBindable(ApibuilderTypeConverter.dateTimeIso8601)

implicit val pathBindableTypeDateIso8601 = ApibuilderPathBindable(ApibuilderTypeConverter.dateIso8601)
implicit val queryStringBindableDateIso8601 = ApibuilderQueryStringBindable(ApibuilderTypeConverter.dateIso8601)
""".trim
  }

  private[models] def buildImplicit(
    enumName: String
  ): String = {
    val fullyQualifiedName = ssd.enumClassName(enumName)
    val converter = s"converter$enumName"
    Seq(
      s"val $converter: ApibuilderTypeConverter[$fullyQualifiedName] = ${buildEnumConverter(enumName)}",
      s"implicit val pathBindable$enumName = ApibuilderPathBindable($converter)",
      s"implicit val queryStringBindable$enumName = ApibuilderQueryStringBindable($converter)"
    ).mkString("\n")
  }

  private[this] def buildEnumConverter(enumName: String): String = {
    val fullyQualifiedName = ssd.enumClassName(enumName)
    Seq(
      s"new ApibuilderTypeConverter[$fullyQualifiedName] {",
      s"  override def convert(value: String): $fullyQualifiedName = $fullyQualifiedName(value)",
      s"  override def convert(value: $fullyQualifiedName): String = value.toString",
      s"  override def example: $fullyQualifiedName = validValues.head",
      s"  override def validValues: Seq[$fullyQualifiedName] = $fullyQualifiedName.all",
      s"}"
    ).mkString("\n")
  }

}
