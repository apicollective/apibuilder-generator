package generator.graphql

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object GraphQLSchemaGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    GraphQLApolloGenerator.invoke(form).map { r =>
      r.headOption.toSeq
    }
  }

}
