package me.apidoc.lib.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator

object Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Right(
      Seq(
        File(
          name = "MeApidocLibAnormParsersUtil.scala",
          dir = Some("me/apidoc/lib"),
          contents = ParsersUtil
        )
      )
    )
  }

  private[this] val ParsersUtil = """
package me.apidoc.lib.anorm.parsers.util {

  sealed trait Config {
    def name(column: String): String
  }

  object Config {
    case class Prefix(prefix: String) extends Config {
      override def name(column: String): String = s"${prefix}_$column"
    }
  }

}
""".trim

}
