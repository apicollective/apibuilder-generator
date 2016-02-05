package go.models

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator

object GoClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val header = ApidocComments(form.service.version, form.userAgent).comments + "\n"

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
