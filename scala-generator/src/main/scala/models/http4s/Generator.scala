package scala.models.http4s

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator
import models.http4s.mock.{Http4s018MockClientGenerator, Http4s020MockClientGenerator, Http4s022MockClientGenerator}

import scala.generator.mock.MockClientGenerator
import scala.generator.{Namespaces, ScalaCaseClasses, ScalaClientMethodConfig, ScalaClientMethodConfigs}
import scala.models.http4s.server.Http4sServer
import scala.models.{ApiBuilderComments, Attributes}

object Http4s015Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s015(namespace, Attributes.Http4sDefaultConfig, baseUrl)
}

object Http4s017Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s017(namespace, Attributes.Http4sDefaultConfig, baseUrl)
}

object Http4s018Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s018(namespace, Attributes.Http4sDefaultConfig, baseUrl)

  override def mkMockClient(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s018MockClientGenerator(ssd, userAgent, config).generateCode()
}

object Http4s020Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s020(namespace, Attributes.Http4sDefaultConfig, baseUrl)

  override def mkMockClient(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s020MockClientGenerator(ssd, userAgent, config).generateCode()
}

object Http4s022Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s022(namespace, Attributes.Http4sDefaultConfig, baseUrl)

  override def mkMockClient(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s022MockClientGenerator(ssd, userAgent, config).generateCode()
}

object Http4s023Generator extends Generator {

  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s023(namespace, Attributes.Http4sDefaultConfig, baseUrl)

  override def mkMockClient(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s022MockClientGenerator(ssd, userAgent, config).generateCode()
}

trait Generator extends CodeGenerator {
  def mkConfig(namespace: String, baseUrl: Option[String]): ScalaClientMethodConfigs.Http4s

  override def invoke(form: InvocationForm): Right[Nothing, Seq[File]] = Right(generateCode(form))

  def mkMockClient(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new MockClientGenerator(ssd, userAgent, config).generateCode()

  private def mkHeader(form: InvocationForm): String =
    ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n"

  private def mkCaseClasses(ssd: ScalaService, form: InvocationForm): String =
    ScalaCaseClasses.generateCode(ssd, form.userAgent, addHeader = false).map(_.contents).mkString("\n\n")

  private def mkJsonCodecs(ssd: ScalaService): String =
    CirceJson(ssd).generate()

  private def mkClient(ssd: ScalaService, form: InvocationForm, config: ScalaClientMethodConfigs.Http4s): String =
    Http4sClient(form, ssd, config).generate()

  private def mkServer(ssd: ScalaService, form: InvocationForm, config: ScalaClientMethodConfigs.Http4s): String =
    Http4sServer(form, ssd, config).generate()

  def generateCode(form: InvocationForm): Seq[File] = {
    val ssd = new ScalaService(form.service, Attributes.Http4sDefaultConfig.withAttributes(form.attributes))
    val config = mkConfig(Namespaces.quote(form.service.namespace), form.service.baseUrl)

    val models = mkCaseClasses(ssd, form)
    val json = mkJsonCodecs(ssd)
    val mock = mkMockClient(form.userAgent, ssd, config)
    val client = mkClient(ssd, form, config)
    val server = mkServer(ssd, form, config)

    val modelsAndJson = s"""$models\n\n$json"""

    val filesToGenerate = List(
      "Client" -> s"""$modelsAndJson\n\n$client""",
      "ModelsJson" -> modelsAndJson,
      "ModelsOnly" -> models,
      "JsonOnly" -> json,
      "ClientOnly" -> client,
      "MockClient" -> mock,
    ) ++
      (if (config.doGenerateServer) List("Server" -> server) else Nil)

    val header = mkHeader(form)

    filesToGenerate
      .map {
        case (suffix, content) =>
          ServiceFileNames.toFile(
            form.service.namespace,
            form.service.organization.key,
            form.service.application.key,
            form.service.version,
            suffix,
            header + content,
            Some("Scala")
          )
      }.toSeq
  }
}
