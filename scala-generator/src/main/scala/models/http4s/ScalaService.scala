package scala.models.http4s

import io.apibuilder.spec.v0.models.Service

import scala.models.Config

class ScalaService(service: Service, config: Config = Config.Http4sDefaultConfig) extends scala.generator.ScalaService(service, config)