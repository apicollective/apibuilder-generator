package scala.models.http4s

import scala.generator.mock.MockClientGenerator

import com.bryzek.apidoc.generator.v0.models.{InvocationForm, File}
import lib.generator.CodeGenerator
import scala.generator.{ScalaService, ScalaCaseClasses, ScalaClientMethodConfigs, Namespaces}
import scala.models.ApidocComments

import generator.ServiceFileNames

object Generator extends Generator

trait Generator extends CodeGenerator {

  override def invoke(form: InvocationForm) = Right(generateCode(form = form, addHeader = true))

  def generateCode(
    form: InvocationForm,
    addHeader: Boolean
  ): Seq[File] = {
    val ssd = ScalaService(form.service)
    val config = ScalaClientMethodConfigs.Http4s(Namespaces.quote(form.service.namespace), form.service.baseUrl)

    val caseClasses = ScalaCaseClasses.generateCode(form, addHeader = false).map(_.contents).mkString("\n\n")
    val json = CirceJson(ssd).generate()
    val client = Http4sClient(form, ssd, config).generate()

    val header = addHeader match {
      case false => ""
      case true => ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
    }

    val mock = header + new MockClientGenerator(form, config).generateCode()

    val modelAndJson =
      s"""$header$caseClasses
         |
         |$json""".stripMargin

    val all =
      s"""$modelAndJson
         |
         |$client""".stripMargin

    Seq(
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "ModelJson", modelAndJson, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", all, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "JsonOnly", json, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "ClientOnly", client, Some("Scala")),
      ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Mock", mock, Some("Scala"))
    )
  }
}
