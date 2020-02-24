package go.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object GoClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Code(form).generate() match {
      case None => {
        Left(Seq("No enums, models, or unions were found and thus no client was generated"))
      }
      case Some(code) => {
        Right(
          Seq(
            File(
              name = GoUtil.packageName(form.service.name) + ".go",
              contents = code
            )
          )
        )
      }
    }
  }

}
