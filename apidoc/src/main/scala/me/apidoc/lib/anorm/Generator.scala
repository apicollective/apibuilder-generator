package me.apidoc.lib.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator

object Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Right(
      Seq(
        ServiceFileNames.toFile(
          form.service.namespace,
          form.service.organization.key,
          form.service.application.key,
          form.service.version,
          "Anorm",
          ParsersUtil,
          Some("Scala")
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
