package scala.models.http4s

import scala.generator.mock.MockClientGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

import scala.generator.{Namespaces, ScalaCaseClasses, ScalaClientMethodConfig, ScalaClientMethodConfigs}
import scala.models.ApidocComments
import scala.models.http4s.server.Http4sServer
import generator.ServiceFileNames
import models.http4s.mock.{Http4s018MockClientGenerator, Http4s019MockClientGenerator}

object Http4s015Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s015(namespace, baseUrl)
}

object Http4s017Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s017(namespace, baseUrl)
}

object Http4s018Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s018(namespace, baseUrl)

  override def generateMockClientCode(form: InvocationForm, ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s018MockClientGenerator(ssd, form.userAgent, config).generateCode()
}

object Http4s019Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s019(namespace, baseUrl)

  override def generateMockClientCode(form: InvocationForm, ssd: ScalaService, config: ScalaClientMethodConfig): String =
    new Http4s019MockClientGenerator(ssd, form.userAgent, config).generateCode()
}

trait Generator extends CodeGenerator {
  def mkConfig(namespace: String, baseUrl: Option[String]): ScalaClientMethodConfigs.Http4s

  def generateMockClientCode(form: InvocationForm, ssd: ScalaService, config: ScalaClientMethodConfig) =
    new MockClientGenerator(ssd, form.userAgent, config).generateCode()

  override def invoke(form: InvocationForm) = Right(generateCode(form = form, addHeader = true))

  def generateCode(
    form: InvocationForm,
    addHeader: Boolean
  ): Seq[File] = {
    val ssd = new ScalaService(form.service)
    val config = mkConfig(Namespaces.quote(form.service.namespace), form.service.baseUrl)

    val header = addHeader match {
      case false => ""
      case true => ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
    }

    val caseClasses = header + ScalaCaseClasses.generateCode(ssd, form.userAgent, addHeader = false).map(_.contents).mkString("\n\n")
    val json = CirceJson(ssd).generate()
    val client = Http4sClient(form, ssd, config).generate()
    val mock = header + generateMockClientCode(form, ssd, config)

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
