package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

import scala.models.Play2Models
import scala.models.{Config, TimeConfig}

object ModelsJson {

    def implicits(ssd: scala.generator.ScalaService) = ssd.config.timeLib match {
      case TimeConfig.JavaTime => Play2Models.javaTimeImplicits
      case TimeConfig.JodaTime => Play2Models.jodaImplicits
    }

    def contents(form: InvocationForm): String = {
        val scalaService = scala.generator.ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))

        val header = scala.models.ApidocComments(form.service.version, form.userAgent)
        val imports = scala.models.JsonImports(form.service)

        val gen = scala.models.Play2Json(scalaService)

        s"""
            ${header.toJavaString()}

            package ${scalaService.namespaces.models}

            package object json {

                import play.api.libs.json.{__, JsString, Writes}
                import play.api.libs.functional.syntax._

                ${imports.mkString("\n")}

                ${implicits(scalaService)}

                ${gen.generateEnums()}
                ${gen.generateModelsAndUnions()}

            }
        """
    }

    def file(form: InvocationForm, contents: String): File =
        generator.ServiceFileNames.toFile(
            form.service.namespace,
            form.service.organization.key,
            form.service.application.key,
            form.service.version,
            "ModelsJson",
            contents,
            Some("Scala")
        )

    def apply(form: InvocationForm): Either[Seq[String], File] = {
        val contents = this.contents(form)
        val file = this.file(form, contents)

        Right(file)
    }
}
