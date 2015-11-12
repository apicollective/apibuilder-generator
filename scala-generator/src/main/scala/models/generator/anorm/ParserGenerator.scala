package scala.generator.anorm

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
// import lib.Text._

object ParserGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    generateCode(form.service) match {
      case None => {
        Left(Seq("No models were found and thus no parsers were generated"))
      }
      case Some(code) => {
        Right(
          Seq(
            File(
              name = "Parsers.scala",
              contents = code
            )
          )
        )
      }
    }
  }

  private[this] def generateCode(service: Service): Option[String] = {
    None
  }

}
