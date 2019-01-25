package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

import scala.models.Play2Models
import scala.models.{Config, TimeConfig}

object ModelsJson {

  def implicits(ssd: scala.generator.ScalaService) = ssd.config.timeLib match {
      case TimeConfig.JavaTime => Play2Models.javaTimeImplicits
      case TimeConfig.JodaTime => Play2Models.jodaImplicits
  }

  def contents(form: InvocationForm): String = {
        val scalaService = scala.generator.ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))

        val header = scala.models.ApidocComments(form.service.version, form.userAgent)
        val imports = scala.models.JsonImports(form.service)

  def jodaLocalDateFormat(): String = s"""
    import play.api.libs.json.JodaReads.DefaultJodaLocalDateReads
    import play.api.libs.json.JodaWrites.DefaultJodaLocalDateWrites
  """

  def contents(form: InvocationForm): String = {
    val scalaService = scala.generator.ScalaService(form.service)
    val imports = scala.models.JsonImports(form.service)
    val gen = scala.models.Play2Json(scalaService)

    s"""
      package ${scalaService.namespaces.models}

      package object json {

        import play.api.libs.json.{__, JsString, Writes}
        import play.api.libs.functional.syntax._

        ${uuidFormat()}
        ${jodaDateTimeFormat()}
        ${jodaLocalDateFormat()}

                ${implicits(scalaService)}

        ${gen.generateEnums()}
        ${gen.generateModelsAndUnions()}

      }
    """
  }

}
