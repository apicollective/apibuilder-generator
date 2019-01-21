package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator._

object JsonFormats extends Component {

  def jsonImports(service: ScalaService): String = {
    (service.namespaces.base +: service.service.imports.map(_.namespace))
      .map { p => s"import ${Namespaces(p).json}._" }
      .mkString("\n")
  }

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val gen = scala.models.Play2Json(service)
    val readersWriters = s"""
      ${gen.generateEnums()}
      ${gen.generateModelsAndUnions()}
    """
      .replaceAll("_root_.org.joda.time.LocalDate", "_root_.java.time.LocalDate")
      .replaceAll("_root_.org.joda.time.DateTime", "_root_.java.time.Instant")
      .replaceAll("_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print", "_root_.java.time.format.DateTimeFormatter.ISO_INSTANT.format")

    val code = s"""
      package ${service.namespaces.json.split('.').dropRight(1).mkString(".")}

      package object ${service.namespaces.json.split('.').last} {
          import play.api.libs.json.{__, JsString, Writes}
          import play.api.libs.functional.syntax._

          import ${service.namespaces.models}._
          ${jsonImports(service)}

          ${readersWriters}
        }
      """

    Validated.validNel(code)
  }
}
