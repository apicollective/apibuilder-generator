package generator.csharp

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object CSharpGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    CSharpGenerator.invoke(form) match {
      case Invalid(errors) => Left(errors.toNonEmptyList.toList)
      case Valid(files) => Right(files)
    }
  }

  val Default: CSharpGenerator = CSharpGenerator()
}

case class CSharpGenerator() {

  def generate(multiService: MultiService): ValidatedNec[String, Seq[File]] = {

  }

}
