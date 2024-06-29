package generator.csharp

import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object CSharpGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    CSharpGenerator().generate(form.service) match {
      case Invalid(errors) => Left(errors.toNonEmptyList.toList)
      case Valid(files) => Right(files)
    }
  }

  val Default: CSharpGenerator = CSharpGenerator()
}

case class CSharpGenerator() {

  def generate(service: Service): ValidatedNec[String, Seq[File]] = {
    if (service.models.isEmpty) {
      "Service does not contain any models".invalidNec
    } else {
      Seq(File(
        name = namespace(service) + ".cs",
        contents = generateNamespace(
          service,
          generateModels(service)
        )
      )).validNec
    }
  }

  private def generateNamespace(service: Service, contents: String): String = {
    Seq(
      s"namespace ${namespace(service)} {",
      contents.trim.indent(2),
      "}"
    ).mkString("\n\n")
  }

  def generateModels(service: Service): String = {
    service.models.map(generateModel).mkString("\n")
  }

  private def generateModel(model: Model): String = {
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

  private def csharpType(typ: String): String = {
    typ match {
      case "boolean" => "bool"
      case "date-iso8601" => "string"
      case "date-time-iso8601" => "string"
      case "decimal" => "decimal"
      case "double" => "double"
      case "integer" => "int"
      case "json" => "object"
      case "long" => "long"
      case "object" => "object"
      case "string" => "string"
      case "unit" => "object"
      case "uuid" => "string"
    }
  }

  /**
   * returns true if the value is 'v0', 'v1', etc indicating a version number
   */
  private def isVersion(value: String): Boolean = {
    if (value.startsWith("v")) {
      value.drop(1).toLongOption.isDefined
    } else {
      false
    }
  }

  private def namespace(service: Service): String = {
    val parts = service.namespace.split("\\.").filterNot(isVersion) ++ Seq(service.name).toList
    Names.pascalCase(parts.distinct.mkString("_"))
  }
}
