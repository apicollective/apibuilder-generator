package generator.openapi

import generator.openapi.aws.{AwsApiGatewayConfig, AwsApiGatewayEnricher}
import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.Service
import sttp.apispec.openapi.OpenAPI

object AwsEnrichment {

  private val AttributeName = "aws_api_gateway"

  def maybeEnrich(openApi: OpenAPI, form: InvocationForm, service: Service): Either[Seq[String], OpenAPI] = {
    form.attributes.find(_.name == AttributeName) match {
      case None => Right(openApi)
      case Some(attr) =>
        AwsApiGatewayConfig.fromJson(attr.value) match {
          case Left(err) => Left(Seq(s"$AttributeName: $err"))
          case Right(config) => Right(AwsApiGatewayEnricher.enrich(openApi, config, service))
        }
    }
  }
}
