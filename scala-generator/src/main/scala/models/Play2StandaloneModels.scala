package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object Play2Scala2StandaloneModelsJson extends PlayStandaloneModelsJson(ScalaVersion(2))
object Play2Scala3StandaloneModelsJson extends PlayStandaloneModelsJson(ScalaVersion(3))

case class PlayStandaloneModelsJson(version: ScalaVersion) extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    Right(Play2Models(version).generateCode(form = form, addBindables = false, addHeader = true, useBuiltInImplicits = false))
  }

}
