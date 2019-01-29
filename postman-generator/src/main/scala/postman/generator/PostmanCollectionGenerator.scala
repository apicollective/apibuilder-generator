package postman.generator

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object PostmanCollectionGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Left(
      Seq("Not implemented yet!")
    )
  }

}
