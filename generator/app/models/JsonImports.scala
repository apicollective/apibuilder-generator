package models

import com.gilt.apidoc.spec.v0.models.Service

/**
  * Given a service, returns a list of all of the imports needed for
  * json serialization/deserialization/
  */
object JsonImports {

  def apply(service: Service): Seq[String] = {
    (
      Seq(s"import ${service.namespace}.models.json._") ++
      service.imports.map { imp =>
        s"import ${imp.namespace}.models.json._"
      }
    ).sorted
  }

}
