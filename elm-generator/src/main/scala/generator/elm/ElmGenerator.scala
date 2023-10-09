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
      val imports = Imports()
      Seq(File(
        name = s"Generated/" + pascalServiceName(service) + ".elm",
        contents = generate(
          service,
          imports,
          generateEnums(service, imports),
          generateModels(service, imports)
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

  private[this] def generate(service: Service, imports: Imports, contents: String*): String = {
    Seq(
      s"module Generated.${pascalServiceName(service)} exposing (..)",
      imports.generateCode(), // must be generated after the content
      contents.mkString("\n\n")
    ).mkString("\n\n")
  }

  private[this] def generateModels(service: Service, imports: Imports): String = {
    val models = ElmModel(imports)
    service.models.map(models.generate).mkString("\n\n")
  }

  private[this] def generateEnums(service: Service, imports: Imports): String = {
    val enums = ElmEnum(imports)
    service.enums.map(enums.generate).mkString("\n\n")
  }
}
