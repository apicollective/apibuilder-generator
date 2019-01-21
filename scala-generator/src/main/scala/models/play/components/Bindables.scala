package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import scala.generator.ScalaService

object Bindables extends Component {

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val bindables = scala.models.Play2Bindables(service)
      .build
      .split("\n")
      .drop(1)
      .dropRight(1)
      .mkString("\n")

    val code = s"""
      package ${service.namespaces.bindables.split('.').dropRight(1).mkString(".")}

      package object ${service.namespaces.bindables.split('.').last} {
        ${bindables}
      }
    """
      // .replaceAll("_root_.org.joda.time.LocalDate", "_root_.java.time.LocalDate")
      // .replaceAll("_root_.org.joda.time.DateTime", "_root_.java.time.Instant")



    Validated.validNel(code)
  }

}
