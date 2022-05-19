package generator.csharp

import cats.data.Validated.{Invalid, Valid}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object CSharpGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    GraphQLCodeGenerator.Default.generate(toMultiService(form)) match {
      case Valid(r) => Right(r.files)
      case Invalid(errors) => Left(errors.toNonEmptyList.toList)
    }
  }

  def toMultiService(form: InvocationForm): MultiService = {
    MultiService(
      (Seq(form.service) ++ form.importedServices.getOrElse(Nil)).map(ApiBuilderService.apply).toList
    )
  }

}
