package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm
import scala.generator.Namespaces

object ModelsJson {

  def uuidFormat(): String = s"""
    import play.api.libs.json.Reads.uuidReads
    import play.api.libs.json.Writes.UuidWrites
  """

  def jodaDateTimeFormat(): String = s"""
    import play.api.libs.json.JodaReads.DefaultJodaDateTimeReads
    import play.api.libs.json.JodaWrites.JodaDateTimeWrites
  """

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

        ${imports.mkString("\n")}

        ${gen.generateEnums()}
        ${gen.generateModelsAndUnions()}

      }
    """
  }

}
