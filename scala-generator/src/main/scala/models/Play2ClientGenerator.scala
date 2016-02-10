package scala.models

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator
import scala.generator.{Namespaces, ScalaClientMethodGenerator, ScalaService, ScalaClientCommon}
import scala.generator.{ScalaCaseClasses, ScalaClientMethodConfig, ScalaClientMethodConfigs}
import generator.ServiceFileNames

case class PlayFrameworkVersion(
  name: String,
  config: ScalaClientMethodConfig,
  requestHolderClass: String,
  authSchemeClass: String,
  supportsHttpPatch: Boolean
)

object Play22ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val config = PlayFrameworkVersion(
      name = "2.2.x",
      config = ScalaClientMethodConfigs.Play22(Namespaces.quote(form.service.namespace), form.service.baseUrl),
      requestHolderClass = "play.api.libs.ws.WS.WSRequestHolder",
      authSchemeClass = "com.ning.http.client.Realm.AuthScheme",
      supportsHttpPatch = false
    )

    Play2ClientGenerator.invoke(config, form)
  }
}

object Play23ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {

    val config = PlayFrameworkVersion(
      name = "2.3.x",
      config = ScalaClientMethodConfigs.Play23(Namespaces.quote(form.service.namespace), form.service.baseUrl),
      requestHolderClass = "play.api.libs.ws.WSRequestHolder",
      authSchemeClass = "play.api.libs.ws.WSAuthScheme",
      supportsHttpPatch = true
    )

    Play2ClientGenerator.invoke(config, form)
  }
}

object Play24ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {

    val config = PlayFrameworkVersion(
      name = "2.4.x",
      config = ScalaClientMethodConfigs.Play24(Namespaces.quote(form.service.namespace), form.service.baseUrl),
      requestHolderClass = "play.api.libs.ws.WSRequest",
      authSchemeClass = "play.api.libs.ws.WSAuthScheme",
      supportsHttpPatch = true
    )

    Play2ClientGenerator.invoke(config, form)
  }
}

object Play2ClientGenerator {

  def invoke(
    version: PlayFrameworkVersion,
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    Play2ClientGenerator(version, form).invoke()
  }

}

case class Play2ClientGenerator(
  version: PlayFrameworkVersion,
  form: InvocationForm
) {

  private[this] val ssd = new ScalaService(form.service)

  def invoke(): Either[Seq[String], Seq[File]] = {
    ScalaCaseClasses.modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(generateCode())
      case errors => Left(errors)
    }
  }

  private def generateCode(): Seq[File] = {
    val source = ApidocComments(form.service.version, form.userAgent).toJavaString + "\n" +
      Seq(
        Play2Models.generateCode(form, addBindables = true, addHeader = false).map(_.contents).mkString("\n\n"),
        client()
      ).mkString("\n\n")

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", source, Some("Scala")))
  }

  private def client(): String = {

    val methodGenerator = new ScalaClientMethodGenerator(version.config, ssd)

    val patchMethod = version.supportsHttpPatch match {
      case true => """_logRequest("PATCH", _requestHolder(path).withQueryString(queryParameters:_*)).patch(body.getOrElse(play.api.libs.json.Json.obj()))"""
      case false => s"""sys.error("PATCH method is not supported in Play Framework Version ${version.name}")"""
    }

    val headers = Headers(form)
    val headerString = headers.scala.
      map { case (name, value) => s""""$name" -> ${value}""" }.
      mkString(".withHeaders(\n        ", ",\n        ", "") + "\n      ).withHeaders(defaultHeaders : _*)"

    s"""package ${ssd.namespaces.base} {

${headers.objectConstants.indent(2)}

${ScalaClientCommon.clientSignature(version.config).indent(2)} {
${JsonImports(form.service).mkString("\n").indent(4)}

    private[this] val logger = play.api.Logger("${ssd.namespaces.base}.Client")

    logger.info(s"Initializing ${ssd.namespaces.base}.Client for url $$apiUrl")

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    def _requestHolder(path: String): ${version.requestHolderClass} = {
      import play.api.Play.current

      val holder = play.api.libs.ws.WS.url(apiUrl + path)$headerString
      auth.fold(holder) {
        case Authorization.Basic(username, password) => {
          holder.withAuth(username, password.getOrElse(""), ${version.authSchemeClass}.BASIC)
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }
    }

    def _logRequest(method: String, req: ${version.requestHolderClass})(implicit ec: scala.concurrent.ExecutionContext): ${version.requestHolderClass} = {
      val queryComponents = for {
        (name, values) <- req.queryString
        value <- values
      } yield s"$$name=$$value"
      val url = s"$${req.url}$${queryComponents.mkString("?", "&", "")}"
      auth.fold(logger.info(s"curl -X $$method $$url")) { _ =>
        logger.info(s"curl -X $$method -u '[REDACTED]:' $$url")
      }
      req
    }

    def _executeRequest(
      method: String,
      path: String,
      queryParameters: Seq[(String, String)] = Seq.empty,
      body: Option[play.api.libs.json.JsValue] = None
    )(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[${version.config.responseClass}] = {
      method.toUpperCase match {
        case "GET" => {
          _logRequest("GET", _requestHolder(path).withQueryString(queryParameters:_*)).get()
        }
        case "POST" => {
          _logRequest("POST", _requestHolder(path).withQueryString(queryParameters:_*).withHeaders("Content-Type" -> "application/json; charset=UTF-8")).post(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PUT" => {
          _logRequest("PUT", _requestHolder(path).withQueryString(queryParameters:_*).withHeaders("Content-Type" -> "application/json; charset=UTF-8")).put(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PATCH" => {
          $patchMethod
        }
        case "DELETE" => {
          _logRequest("DELETE", _requestHolder(path).withQueryString(queryParameters:_*)).delete()
        }
         case "HEAD" => {
          _logRequest("HEAD", _requestHolder(path).withQueryString(queryParameters:_*)).head()
        }
         case "OPTIONS" => {
          _logRequest("OPTIONS", _requestHolder(path).withQueryString(queryParameters:_*)).options()
        }
        case _ => {
          _logRequest(method, _requestHolder(path).withQueryString(queryParameters:_*))
          sys.error("Unsupported method[%s]".format(method))
        }
      }
    }

  }

${ScalaClientCommon(version.config).indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}

}"""
  }

}
