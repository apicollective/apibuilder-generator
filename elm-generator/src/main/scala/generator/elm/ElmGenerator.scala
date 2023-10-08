package generator.elm

import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
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
        name = moduleName(service) + ".elm",
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

  private[this] def moduleName(service: Service): String = {
    val parts = service.namespace.split("\\.").filterNot(isVersion) ++ Seq(service.name).toList
    Names.pascalCase(parts.distinct.mkString("_"))
  }

  private[this] def generate(service: Service, contents: String): String = {
    Seq(
      s"module ${moduleName(service)} exposing (..)",
      contents.trim
    ).mKString("\n\n")
  }

  def generateModels(service: Service): String = {
    service.models.map(generateModel).mkString("\n")
  }

  private[this] def generateModel(model: Model): String = {
    model.fields.foldLeft(
      RecordBuilder().withName(Names.pascalCase(model.name))
    ) { case (b, f) =>
      b.withField(
        RecordField(
          name = Names.pascalCase(f.name),
          `type` = csharpType(f.`type`),
          required = f.required
        )
      )
    }.build
  }
}
