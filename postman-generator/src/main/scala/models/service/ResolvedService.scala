package models.service

import io.apibuilder.spec.v0.models.{Resource, Service}

case class ResolvedService(service: Service, serviceNamespaceToResources: Map[String, Seq[Resource]])