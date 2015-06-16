package lib.generator

import com.bryzek.apidoc.generator.v0.models.InvocationForm

trait CodeGenerator {

  /**
    * Invokes the code generators, returning either a list of errors
    * or the result of the code generation.
    */
  def invoke(form: InvocationForm): Either[Seq[String], String]

}
