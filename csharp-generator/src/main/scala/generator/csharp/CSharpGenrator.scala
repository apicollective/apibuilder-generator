package generator.csharp

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object CSharpGenrator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    CSharpGenerator.invoke(form).map { r =>
      r.headOption.toSeq
    }
  }

}
