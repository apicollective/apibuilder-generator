package generator.csharp

import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Service
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
    println(s"service: ${service.namespace}")
    "Generator not yet ready".invalidNec
  }

}
