package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object Play2StandaloneModelsJson extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    Right(Play2ModelsScala2.generateCode(form = form, addBindables = false, addHeader = true, useBuiltInImplicits = false))
  }

}
