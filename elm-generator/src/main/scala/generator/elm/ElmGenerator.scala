package generator.elm

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.generator.CodeGenerator

object ElmGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    ElmGenerator().generate(form.service) match {
      case Invalid(errors) => Left(errors.toNonEmptyList.toList)
      case Valid(files) => Right(files)
    }
  }

  val Default: ElmGenerator = ElmGenerator()
}

case class ElmGenerator() {

  def generate(service: Service): ValidatedNec[String, Seq[File]] = {
    if (service.models.isEmpty) {
      "Service does not contain any models".invalidNec
    } else {
      Seq(File(
        name = s"Generated/" + pascalServiceName(service) + ".elm",
        contents = generate(
          service,
          generateModels(service)
        )
      )).validNec
    }
  }


  /**
   * returns true if the value is 'v0', 'v1', etc indicating a version number
   */
  private[this] def isVersion(value: String): Boolean = {
    if (value.startsWith("v")) {
      value.drop(1).toLongOption.isDefined
    } else {
      false
    }
  }

  private[this] def pascalServiceName(service: Service): String = {
    val parts = service.namespace.split("\\.").filterNot(isVersion).toList
    Names.pascalCase(parts.distinct.mkString("_"))
  }

  private[this] def generate(service: Service, contents: String): String = {
    Seq(
      s"module Generated.${pascalServiceName(service)} exposing (..)",
      contents.trim
    ).mkString("\n\n")
  }

  def generateModels(service: Service): String = {
    service.models.map(TypeAlias.generate).mkString("\n\n")
  }
}
