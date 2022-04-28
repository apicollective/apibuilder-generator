package scala.models

import lib.Text._

import scala.generator.{ScalaEnumValue, ScalaPrimitive, ScalaService, ScalaUtil}

case class Play2Bindables(ssd: ScalaService) {

  def build(): String = {
    Seq(
      "object Bindables {",
      Seq(
        "import play.api.mvc.{PathBindable, QueryStringBindable}",
        buildImports(),
        buildObjectCore(ssd.attributes.dateTimeType.dataType, ssd.attributes.dateType.dataType),
        buildObjectModels().getOrElse(""),
        apibuilderHelpers(ssd.attributes.dateTimeType.dataType, ssd.attributes.dateType.dataType)
      ).filter(_.trim.nonEmpty).mkString("\n\n").indentString(2),
      "}"
    ).mkString("\n\n")
  }

  private def buildObjectCore(dateTimeIso8601: ScalaPrimitive, dateIso8601: ScalaPrimitive): String = {
    s"""object Core {
       |  implicit def pathBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[${dateTimeIso8601.fullName}] = ApibuilderPathBindable(ApibuilderTypes.dateTimeIso8601)
       |  implicit def queryStringBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[${dateTimeIso8601.fullName}] = ApibuilderQueryStringBindable(ApibuilderTypes.dateTimeIso8601)
       |
       |  implicit def pathBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[${dateIso8601.fullName}] = ApibuilderPathBindable(ApibuilderTypes.dateIso8601)
       |  implicit def queryStringBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[${dateIso8601.fullName}] = ApibuilderQueryStringBindable(ApibuilderTypes.dateIso8601)
       |}""".stripMargin
  }

  private def buildObjectModels(): Option[String] = {
    if (ssd.enums.isEmpty) {
      None
    } else {
      Some(
        Seq(
          s"object Models {",
          Seq(
            ssd.namespaces.importStatements().sorted.mkString("\n"),
            ssd.enums.map { e => buildImplicit(e.name) }.mkString("\n\n")
          ).filter(_.nonEmpty).mkString("\n\n").indentString(2),
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

  private def apibuilderHelpers(dateTimeIso8601: ScalaPrimitive, dateIso8601: ScalaPrimitive): String = {
    s"""trait ApibuilderTypeConverter[T] {
       |
       |  def convert(value: String): T
       |
       |  def convert(value: T): String
       |
       |  def example: T
       |
       |  def validValues: Seq[T] = Nil
       |
       |  def errorMessage(key: String, value: String, ex: java.lang.Exception): String = {
       |    val base = s"Invalid value '$$value' for parameter '$$key'. "
       |    validValues.toList match {
       |      case Nil => base + "Ex: " + convert(example)
       |      case values => base + ". Valid values are: " + values.mkString("'", "', '", "'")
       |    }
       |  }
       |}
       |
       |object ApibuilderTypes {
       |  val dateTimeIso8601: ApibuilderTypeConverter[${dateTimeIso8601.fullName}] = new ApibuilderTypeConverter[${dateTimeIso8601.fullName}] {
       |    override def convert(value: String): ${dateTimeIso8601.fullName} = ${dateTimeIso8601.fromStringValue("value")}
       |    override def convert(value: ${dateTimeIso8601.fullName}): String = ${dateTimeIso8601.asString("value")}
       |    override def example: ${dateTimeIso8601.fullName} = ${dateTimeIso8601.fullName}.now
       |  }
       |
       |  val dateIso8601: ApibuilderTypeConverter[${dateIso8601.fullName}] = new ApibuilderTypeConverter[${dateIso8601.fullName}] {
       |    override def convert(value: String): ${dateIso8601.fullName} = ${dateIso8601.fromStringValue("value")}
       |    override def convert(value: ${dateIso8601.fullName}): String = ${dateIso8601.asString("value")}
       |    override def example: ${dateIso8601.fullName} = ${dateIso8601.fullName}.now
       |  }
       |}
       |
       |final case class ApibuilderQueryStringBindable[T](
       |  converters: ApibuilderTypeConverter[T]
       |) extends QueryStringBindable[T] {
       |
       |  override def bind(key: String, params: Map[String, Seq[String]]): _root_.scala.Option[_root_.scala.Either[String, T]] = {
       |    params.getOrElse(key, Nil).headOption.map { v =>
       |      try {
       |        Right(
       |          converters.convert(v)
       |        )
       |      } catch {
       |        case ex: java.lang.Exception => Left(
       |          converters.errorMessage(key, v, ex)
       |        )
       |      }
       |    }
       |  }
       |
       |  override def unbind(key: String, value: T): String = {
       |    s"$$key=$${converters.convert(value)}"
       |  }
       |}
       |
       |final case class ApibuilderPathBindable[T](
       |  converters: ApibuilderTypeConverter[T]
       |) extends PathBindable[T] {
       |
       |  override def bind(key: String, value: String): _root_.scala.Either[String, T] = {
       |    try {
       |      Right(
       |        converters.convert(value)
       |      )
       |    } catch {
       |      case ex: java.lang.Exception => Left(
       |        converters.errorMessage(key, value, ex)
       |      )
       |    }
       |  }
       |
       |  override def unbind(key: String, value: T): String = {
       |    converters.convert(value)
       |  }
       |}""".stripMargin
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
    val `enum` = ssd.enums.find(_.qualifiedName == fullyQualifiedName).getOrElse {
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
