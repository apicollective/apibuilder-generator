package models.http4s.mock

import scala.generator.{ScalaClientMethodConfig, ScalaResource, ScalaService}
import scala.generator.mock.MockClientGenerator
import lib.Text._

class Cats1MockClientGenerator(
  ssd: ScalaService,
  userAgent: Option[String],
  config: ScalaClientMethodConfig
) extends MockClientGenerator(ssd, userAgent, config) {

  override val clientCode: String = {
    Seq(
      s"class Client[F[_]: cats.Applicative] extends ${ssd.namespaces.interfaces}.Client[F] {",
      s"""  ${config.formatBaseUrl(Some("http://mock.localhost"))}""",
      ssd.resources.map { resource =>
        s"override def ${generator.methodName(resource)}: ${ssd.namespaces.base}.${resource.plural}[F] = new Mock${resource.plural}[F]"
      }.mkString("\n").indent(2),
      "}",
      ssd.resources.map { resource =>
        generateMockResource(resource)
      }.mkString("\n\n")
    ).mkString("\n\n")
  }

  override def generateMockResource(resource: ScalaResource): String = {
    Seq(
      s"class Mock${resource.plural}[F[_]: cats.Applicative] extends ${ssd.namespaces.base}.${resource.plural}[F] {",
      generator.methods(resource).map { m =>
        Seq(
          m.interface + s" = cats.Applicative[F].pure {",
          mockImplementation(m).indent(2),
          "}"
        ).mkString("\n")
      }.mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

}
