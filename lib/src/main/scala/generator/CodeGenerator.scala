package lib.generator

import io.apibuilder.generator.v0.models.{File, InvocationForm}

trait CodeGenerator {

  /**
    * Invokes the code generators, returning either a list of errors
    * or the result of the code generation.
    */
  def invoke(form: InvocationForm): Either[Seq[String], Seq[File]]

}
