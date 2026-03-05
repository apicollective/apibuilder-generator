package generator.openapi

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

object OpenApiYamlGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val service = form.service
    val importedServices = form.importedServices.getOrElse(Nil)
    val openApi = OpenApiConverter.convert(service, importedServices)

    AwsExtension.maybeEnrich(openApi, form, service).map { enrichedOpenApi =>
      val yaml = OutputWriter.toYaml(enrichedOpenApi)
      Seq(
        ServiceFileNames.toFile(
          namespace = service.namespace,
          organizationKey = service.organization.key,
          applicationKey = service.application.key,
          version = service.version,
          suffix = "OpenApi",
          contents = yaml,
          languages = Some("text"),
        )
      )
    }
  }
}
