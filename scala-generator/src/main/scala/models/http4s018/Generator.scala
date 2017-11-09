package models.http4s018

import scala.generator.mock.MockClientGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.generator.CodeGenerator

import scala.generator.{Namespaces, ScalaCaseClasses, ScalaClientMethodConfigs}
import scala.models.ApidocComments
import generator.ServiceFileNames
import models.http4s018.server.Http4sServer

object Http4s018Generator extends Generator {
  override def mkConfig(namespace: String, baseUrl: Option[String]) = ScalaClientMethodConfigs.Http4s018(namespace, baseUrl)
}

trait Generator extends CodeGenerator {
  def mkConfig(namespace: String, baseUrl: Option[String]): ScalaClientMethodConfigs.Http4s

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
    val mock = header + new MockClientGenerator(ssd, form.userAgent, config).generateCode()

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
