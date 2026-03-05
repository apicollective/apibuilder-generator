package generator.openapi.aws

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder

case class AwsApiGatewayIntegration(
  `type`: String,
  domain: String = "",
  protocol: String = "http",
  connectionType: Option[String] = None,
  connectionId: Option[String] = None,
)

case class AwsApiGatewayAuthorizer(
  name: String,
  uri: String,
  identitySource: String = "method.request.header.Authorization",
  ttl: Option[Int] = None,
)

case class AwsApiGatewayRequestParameter(
  from: String,
  to: String,
)

case class AwsApiGatewayConfig(
  integration: AwsApiGatewayIntegration,
  authorizer: Option[AwsApiGatewayAuthorizer] = None,
  requestParameters: Seq[AwsApiGatewayRequestParameter] = Nil,
)

object AwsApiGatewayConfig {

  implicit val config: Configuration = Configuration.default.withDefaults

  implicit val integrationDecoder: Decoder[AwsApiGatewayIntegration] = deriveConfiguredDecoder
  implicit val authorizerDecoder: Decoder[AwsApiGatewayAuthorizer] = deriveConfiguredDecoder
  implicit val requestParameterDecoder: Decoder[AwsApiGatewayRequestParameter] = deriveConfiguredDecoder
  implicit val configDecoder: Decoder[AwsApiGatewayConfig] = deriveConfiguredDecoder

  def fromJson(jsonString: String): Either[String, AwsApiGatewayConfig] = {
    io.circe.parser.parse(jsonString) match {
      case Left(err) => Left(s"Failed to parse JSON: ${err.getMessage}")
      case Right(json) =>
        json.as[AwsApiGatewayConfig] match {
          case Left(err) => Left(s"Failed to decode aws_api_gateway config: ${err.getMessage}")
          case Right(cfg) => Right(cfg)
        }
    }
  }
}
