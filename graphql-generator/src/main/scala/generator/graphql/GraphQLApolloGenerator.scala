package generator.graphql

import cats.data.Validated.{Invalid, Valid}
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.graphql.GraphQLCodeGenerator
import io.apibuilder.validation.{ApiBuilderService, MultiService}
import lib.generator.CodeGenerator

object GraphQLApolloGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    GraphQLCodeGenerator.Default.generate(toMultiService(form)) match {
      case Valid(r) => Right(r.invocation.files)
      case Invalid(errors) => Left(errors.toNonEmptyList.toList)
    }
  }

  def toMultiService(form: InvocationForm): MultiService = {
    MultiService(
      (Seq(form.service) ++ form.importedServices.getOrElse(Nil)).map(ApiBuilderService.apply).toList
    )
  }

}
