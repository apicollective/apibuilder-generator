package scala.models.http4s

import io.apibuilder.spec.v0.models.Service

import scala.models.Attributes

class ScalaService(service: Service, config: Attributes = Attributes.Http4sDefaultConfig) extends scala.generator.ScalaService(service, config)
