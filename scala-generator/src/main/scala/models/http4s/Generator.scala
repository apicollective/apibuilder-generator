package scala.models.http4s

import scala.generator.mock.MockClientGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

import scala.generator.{Namespaces, ScalaCaseClasses, ScalaClientMethodConfig, ScalaClientMethodConfigs}
import scala.models.{ApiBuilderComments, Attributes}
import scala.models.http4s.server.Http4sServer
import generator.ServiceFileNames
import models.http4s.mock.{Http4s018MockClientGenerator, Http4s020MockClientGenerator}

object Http4s015Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s015(namespace, Attributes.Http4sDefaultConfig, baseUrl)
}

object Http4s017Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s017(namespace, Attributes.Http4sDefaultConfig, baseUrl)
}

object Http4s018Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s018(namespace, Attributes.Http4sDefaultConfig, baseUrl)

  override def generateMockClientCode(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s018MockClientGenerator(ssd, userAgent, config).generateCode()
}

object Http4s020Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s020(namespace, Attributes.Http4sDefaultConfig, baseUrl)

  override def generateMockClientCode(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s020MockClientGenerator(ssd, userAgent, config).generateCode()
}

trait Generator extends CodeGenerator {
  def mkConfig(namespace: String, baseUrl: Option[String]): ScalaClientMethodConfigs.Http4s

  def generateMockClientCode(userAgent: Option[String], ssd: ScalaService, config: ScalaClientMethodConfig) =
    new MockClientGenerator(ssd, userAgent, config).generateCode()

  override def invoke(form: InvocationForm) = Right(generateCode(form = form, addHeader = true))

  def generateCode(
    form: InvocationForm,
    addHeader: Boolean
  ): Seq[File] = {
    val ssd = new ScalaService(form.service, Attributes.Http4sDefaultConfig.withAttributes(form.attributes))
    val config = mkConfig(Namespaces.quote(form.service.namespace), form.service.baseUrl)

    val header = addHeader match {
      case false => ""
      case true => ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n"
    }

    val caseClasses = header + ScalaCaseClasses.generateCode(ssd, form.userAgent, addHeader = false).map(_.contents).mkString("\n\n")
    val json = CirceJson(ssd).generate()
    val client = Http4sClient(form, ssd, config).generate()
    val mock = header + generateMockClientCode(form.userAgent, ssd, config)

    val modelAndJson =
      s"""$caseClasses
         |
         |$json""".stripMargin

    val all =
      s"""$modelAndJson
         |
         |$client""".stripMargin

    val server = header + Http4sServer(form, ssd, config).generate()

    Seq(
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", all, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "ModelsJson", modelAndJson, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "ModelsOnly", caseClasses, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "JsonOnly", s"$header$json", Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "ClientOnly", s"$header$client", Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "MockClient", mock, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Server", server, Some("Scala"))
    )
  }
}
