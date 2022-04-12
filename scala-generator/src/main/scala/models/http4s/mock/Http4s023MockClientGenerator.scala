package models.http4s.mock

import lib.Text._

import scala.generator.mock.MockClientGenerator
import scala.generator.{ScalaClientMethodConfig, ScalaResource, ScalaService}

class Http4s023MockClientGenerator(
  ssd: ScalaService,
  userAgent: Option[String],
  config: ScalaClientMethodConfig
) extends MockClientGenerator(ssd, userAgent, config) {

  override def clientCode: String = {
    Seq(
      s"import cats.effect.kernel.Concurrent\n\n" +
      s"class Client[F[_]: cats.Applicative] extends ${ssd.namespaces.interfaces}.Client[F] {",
      s"""  ${config.formatBaseUrl(Some("http://mock.localhost"))}""",
      ssd.resources.map { resource =>
        s"override def ${generator.methodName(resource)}: ${ssd.namespaces.base}.${resource.plural}[F] = new Mock${resource.plural}[F]"
      }.mkString("\n").indentString(2),
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
          mockImplementation(m).indentString(2),
          "}"
        ).mkString("\n")
      }.mkString("\n\n").indentString(2),
      "}"
    ).mkString("\n\n")
  }

}
