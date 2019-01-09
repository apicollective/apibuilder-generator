package scala.models.play

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object Play26Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = for {
      routers <- generators.ScalaSirdRouter.invoke(form)
  } yield routers

}
