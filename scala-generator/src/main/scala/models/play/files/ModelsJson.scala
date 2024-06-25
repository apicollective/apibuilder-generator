package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm

import scala.models.{Attributes, DateTimeTypeConfig, DateTypeConfig, Play2ModelImplicits, Play2Models}

object ModelsJson {

  private def implicits(ssd: scala.generator.ScalaService) = Seq(Play2ModelImplicits.timeImplicits) ++
    (ssd.attributes.dateTimeType match {
      case DateTimeTypeConfig.JodaDateTime => Seq(Play2ModelImplicits.jodaDateTimeImplicits)
      case _ => Nil
    }) ++
    (ssd.attributes.dateType match {
      case DateTypeConfig.JodaLocalDate => Seq(Play2ModelImplicits.jodaLocalDateImplicits)
      case _ => Nil
    })

  def contents(form: InvocationForm, scala3Support: Boolean): String = {
    val scalaService = scala.generator.ScalaService(form.service, Attributes.PlayGen2DefaultConfig.withAttributes(form.attributes))
    val imports = scala.models.JsonImports(form.service)
    val gen = scala.models.Play2Json(scalaService, scala3Support = scala3Support)

    s"""
      package ${scalaService.namespaces.models}

      package object json {

        import play.api.libs.json.{__, JsString, Writes}
        import play.api.libs.functional.syntax._

        ${implicits(scalaService).mkString("\n")}

        ${imports.mkString("\n")}

        ${gen.generateEnums()}
        ${gen.generateModelsAndUnions()}

      }
    """
  }

}
