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
        name = toFileName(service),
        contents = service.models.map(generateCode).mkString("\n")
      )).validNec
    }
  }

  private[this] def toFileName(service: Service): String = {
    Names.pascalCase(
      (service.namespace.split("\\.").filterNot(_=="v0") ++ Seq(service.name)).mkString("_") + ".cs"
    )
  }

  private[this] def generateCode(model: Model): String = {
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

  private[this] def csharpType(typ: String): String = {
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

}
