package scala.generator.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
// import lib.Text._

object ParserGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Right(
      Seq(
        File(
          name = "Parsers.scala",
          contents = "TODO"
        )
      )
    )
  }

}
