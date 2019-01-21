package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator.ScalaService

object Models extends Component {

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val caseClasses = scala.generator.ScalaCaseClasses
      .generateCodeBody(service)

    val code = s"""
      package ${service.namespaces.models.split('.').dropRight(1).mkString(".")}

      package object ${service.namespaces.models.split('.').last} {
        ${caseClasses}
      }
    """
      .replaceAll("_root_.org.joda.time.LocalDate", "_root_.java.time.LocalDate")
      .replaceAll("_root_.org.joda.time.DateTime", "_root_.java.time.Instant")

    Validated.validNel(code)
  }

}
