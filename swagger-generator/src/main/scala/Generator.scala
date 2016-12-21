package swagger

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import com.bryzek.apidoc.swagger.v0.models._
import com.bryzek.apidoc.swagger.v0.models.json._
import lib.Text._
import lib.generator.CodeGenerator
import generator.ServiceFileNames
import play.api.libs.json.Json

object Generator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Swagger20(form).invoke
  }
}

case class Swagger20(form: InvocationForm) {

  def invoke(): Either[Seq[String], Seq[File]] = {
    val swagger = toSwagger(form.service)

    Right(
      Seq(
        ServiceFileNames.toFile(
          form.service.namespace,
          form.service.organization.key,
          form.service.application.key,
          form.service.version,
          "Swagger",
          Json.toJson(swagger).toString,
          Some("Swagger")
        )
      )
    )
  }

  def toSwagger(service: Service): Swagger = { 
    Swagger(
      swagger = "2.0",
      host = service.baseUrl.map { host(_) }.getOrElse("N/A"),
      basePath = service.baseUrl.map { path(_) }.getOrElse("/")
    )
  }

  def host(url: String): String = {
    new java.net.URL(url).getHost
  }

  def path(url: String): String = {
    new java.net.URL(url).getPath
  }

}
