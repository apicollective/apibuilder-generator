package generator.openapi

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.validation.{ApiBuilderService, MultiService}
import sttp.apispec.openapi._

object OpenApiConverter {

  def convert(service: Service, importedServices: Seq[Service]): OpenAPI = {
    val allServices = (Seq(service) ++ importedServices).map(ApiBuilderService.apply).toList
    val multiService = MultiService(allServices)
    buildOpenApi(service, multiService)
  }

  private def buildOpenApi(service: Service, multiService: MultiService): OpenAPI = {
    val primaryAbs = ApiBuilderService(service)
    val combined = if (multiService.services.exists(_.namespace == service.namespace)) {
      multiService
    } else {
      MultiService(primaryAbs :: multiService.services)
    }
    val filtered = MultiServiceExtractor(combined).extractReferencedTypes(service)

    val resolver = new TypeResolver(filtered)

    val schemas = SchemaConverter.convert(filtered, resolver)
    val paths = PathConverter.convert(service.resources, resolver)
    val headerResult = HeaderConverter.convert(service.headers)

    val components = if (schemas.nonEmpty || headerResult.securitySchemes.nonEmpty) {
      Some(
        Components(
          schemas = schemas,
          securitySchemes = headerResult.securitySchemes,
        ),
      )
    } else {
      None
    }

    val servers = service.baseUrl.map(url => Server(url = url)).toList

    OpenAPI(
      openapi = "3.0.3",
      info = Info(
        title = service.name,
        version = service.version,
        description = service.description,
      ),
      servers = servers,
      paths = paths,
      components = components,
      security = if (headerResult.globalSecurity.nonEmpty) headerResult.globalSecurity else Nil,
    )
  }
}
