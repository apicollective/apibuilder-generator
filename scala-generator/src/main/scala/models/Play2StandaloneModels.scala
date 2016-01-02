package scala.models

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import scala.generator.ScalaCaseClasses
import lib.generator.CodeGenerator

object Play2StandaloneModelsJson extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    ScalaCaseClasses.modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(Play2Models.generateCode(form = form, addBindables = false, addHeader = true))
      case errors => Left(errors)
    }
  }

}
