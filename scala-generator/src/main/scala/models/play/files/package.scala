package scala.models.play.files

import cats.data.ValidatedNel
import cats.implicits._
import io.apibuilder.generator.v0.models.{File => ModelFile}
import scala.generator.ScalaService
import scala.models.play.components
import scala.models.play.Helpers._

class Bindables(scalaService: ScalaService) extends File(scalaService.namespaces.base, "Bindables.scala", new components.Bindables(scalaService))
class BodyParsers(scalaService: ScalaService) extends File(scalaService.namespaces.base, "BodyParsers.scala", new components.BodyParsers(scalaService))
class Controllers(scalaService: ScalaService) extends File(scalaService.namespaces.base, "Controllers.scala", new components.Controllers(scalaService))
class Json(scalaService: ScalaService) extends File(scalaService.namespaces.base, "Json.scala", new components.Json(scalaService))
class SirdRouters(scalaService: ScalaService) extends File(scalaService.namespaces.base, "SirdRouters.scala", new components.SirdRouters(scalaService))

abstract class File(packageName: String, name: String, component: components.Component) {
    def file: ValidatedNel[String, ModelFile] =
        content.map(content => ModelFile(name, contents = content))

    def content(): ValidatedNel[String, String] =
        component.code.map { code =>
            s"""
            |package ${packageName}
            |
            |${code}
            """.clean
        }
}
