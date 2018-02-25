package scala.models

import scala.generator.{ScalaEnumValue, ScalaService}

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
    converters.convert(value)
  }
}

case class ApibuilderQueryStringBindableOption[T](
  bindable: QueryStringBindable[T]
) extends QueryStringBindable[_root_scala.Option[T]] {

  override def bind(key: String, params: Map[String, Seq[String]]): _root_.scala.Option[_root_.scala.Either[String, _root_scala.Option[T]]] = {
    params.getOrElse(key, Nil).headOption match {
      case None => Some(Right(None))
      case Some(v) => bindable.bind(key, params).map {
        case Left(errors) => Left(errors)
        case Right(parsed) => Right(Some(parsed))
      }
    }
  }

  override def unbind(key: String, value: _root_scala.Option[T]): String = {
    value match {
      case None => ""
      case Some(v) => bindable.unbind(key, v)
    }
  }
}

case class ApibuilderPathBindable[T](
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

implicit val pathBindableApibuilderDateTimeIso8601: PathBindable[_root_.org.joda.time.DateTime] = ApibuilderPathBindable(ApibuilderTypeConverter.dateTimeIso8601)
implicit val queryStringBindableApibuilderDateTimeIso8601: QueryStringBindable[_root_.org.joda.time.DateTime] = ApibuilderQueryStringBindable(ApibuilderTypeConverter.dateTimeIso8601)
implicit val queryStringBindableApibuilderDateTimeIso8601Option: QueryStringBindable[_root_.scala.Option[_root_.org.joda.time.DateTime]] = ApibuilderQueryStringBindableOption(queryStringBindableApibuilderDateTimeIso8601)

implicit val pathBindableApibuilderDateIso8601: PathBindable[_root_.org.joda.time.LocalDate] = ApibuilderPathBindable(ApibuilderTypeConverter.dateIso8601)
implicit val queryStringBindableApibuilderDateIso8601: QueryStringBindable[_root_.org.joda.time.LocalDate] = ApibuilderQueryStringBindable(ApibuilderTypeConverter.dateIso8601)
implicit val queryStringBindableApibuilderDateIso8601Option: QueryStringBindable[_root_.scala.Option[_root_.org.joda.time.LocalDate]] = ApibuilderQueryStringBindableOption(queryStringBindableApibuilderDateIso8601)
""".trim
  }

  private[models] def buildImplicit(
    enumName: String
  ): String = {
    val fullyQualifiedName = ssd.enumClassName(enumName)
    val converter = s"converter$enumName"
    Seq(
      s"val $converter: ApibuilderTypeConverter[$fullyQualifiedName] = ${buildEnumConverter(enumName)}",
      s"implicit val pathBindable$enumName: PathBindable[$fullyQualifiedName] = ApibuilderPathBindable($converter)",
      s"implicit val queryStringBindable$enumName: QueryStringBindable[$fullyQualifiedName] = ApibuilderQueryStringBindable($converter)",
      s"implicit val queryStringBindable${enumName}Option: QueryStringBindable[Option[$fullyQualifiedName]] = ApibuilderQueryStringBindableOption(queryStringBindable$enumName)"
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
