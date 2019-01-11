package scala.models.play.files

import cats.data.ValidatedNel
import cats.implicits._
import io.apibuilder.generator.v0.models.{File => ModelFile}
import scala.generator.ScalaService
import scala.models.play.components
import scala.models.play.Helpers._

class Bindables(scalaService: ScalaService) extends File(scalaService, "Bindables", new components.Bindables(scalaService))
class BodyParsers(scalaService: ScalaService) extends File(scalaService, "BodyParsers", new components.BodyParsers(scalaService))
class Controllers(scalaService: ScalaService) extends File(scalaService, "Controllers", new components.Controllers(scalaService))
class JsonFormats(scalaService: ScalaService) extends File(scalaService, "JsonFormats", new components.JsonFormats(scalaService))
class Models(scalaService: ScalaService) extends File(scalaService, "Models", new components.Models(scalaService))
class SirdRouters(scalaService: ScalaService) extends File(scalaService, "SirdRouters", new components.SirdRouters(scalaService))

abstract class File(scalaService: ScalaService, name: String, component: components.Component) {
    def file: ValidatedNel[String, ModelFile] =
        content.map { content =>
            generator.ServiceFileNames.toFile(
                scalaService.service.namespace,
                scalaService.service.organization.key,
                scalaService.service.application.key,
                scalaService.service.version,
                name,
                content,
                Some("Scala")
            )
        }

    def content(): ValidatedNel[String, String] =
        component.code.map { code =>
            s"""
            |package ${scalaService.namespaces.base}
            |
            |${code}
            """.clean
        }
}
