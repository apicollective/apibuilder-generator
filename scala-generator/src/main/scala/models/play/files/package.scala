package scala.models.play.files

import cats.data.ValidatedNel
import cats.implicits._
import io.apibuilder.generator.v0.models.{File => ModelFile}
import scala.generator.ScalaService
import scala.models.play.components

class Bindables(scalaService: ScalaService) extends File(scalaService, "Bindables", components.Bindables)
class BodyParsers(scalaService: ScalaService) extends File(scalaService, "BodyParsers", components.BodyParsers)
class Controllers(scalaService: ScalaService) extends File(scalaService, "Controllers", components.Controllers)
class JsonFormats(scalaService: ScalaService) extends File(scalaService, "JsonFormats", components.JsonFormats)
class Models(scalaService: ScalaService) extends File(scalaService, "Models", components.Models)
class Results(scalaService: ScalaService) extends File(scalaService, "Results", components.Results)
class ResultsForPlay(scalaService: ScalaService) extends File(scalaService, "ResultsForPlay", components.ResultsForPlay)
class SirdRouters(scalaService: ScalaService) extends File(scalaService, "SirdRouters", components.SirdRouters)

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
        component.code(scalaService)
}
