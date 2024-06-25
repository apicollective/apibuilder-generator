package scala.models

import scala.generator.Namespaces
import io.apibuilder.spec.v0.models.Service

/**
  * Given a service, returns a list of all of the imports needed for
  * json serialization/deserialization/
  */
object JsonImports {

  def apply(service: Service, includeSelf: Boolean = true): Seq[String] = {
    val namespaces  = (Seq(service.namespace) ++ service.imports.map(_.namespace)).sorted.distinct.filter { ns =>
      includeSelf || ns != service.namespace
    }
    namespaces.map { ns => Namespaces(ns).models }.map { ns => s"import ${ns}.json._" }
  }

}
