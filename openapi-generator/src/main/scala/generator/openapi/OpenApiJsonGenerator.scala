package generator.openapi

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object OpenApiJsonGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val service = form.service
    val importedServices = form.importedServices.getOrElse(Nil)
    val openApi = OpenApiConverter.convert(service, importedServices)

    AwsEnrichment.maybeEnrich(openApi, form, service).map { enrichedOpenApi =>
      val json = OutputWriter.toJson(enrichedOpenApi, pretty = true)
      Seq(
        ServiceFileNames.toFile(
          namespace = service.namespace,
          organizationKey = service.organization.key,
          applicationKey = service.application.key,
          version = service.version,
          suffix = "OpenApi",
          contents = json,
          languages = Some("json"),
        )
      )
    }
  }
}
