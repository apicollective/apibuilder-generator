package models

import com.gilt.apidoc.spec.v0.models.Service
import com.gilt.apidoc.generator.v0.models.InvocationForm
import lib.VersionTag
import lib.Text._
import generator.{Namespaces, ScalaClientMethodGenerator, ScalaService, CodeGenerator, ScalaClientCommon}
import generator.{ScalaClientMethodConfig, ScalaClientMethodConfigs}

case class PlayFrameworkVersion(
  name: String,
  config: ScalaClientMethodConfig,
  requestHolderClass: String,
  authSchemeClass: String,
  supportsHttpPatch: Boolean
)

object Play22ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): String = {
    val config = PlayFrameworkVersion(
      name = "2.2.x",
      config = ScalaClientMethodConfigs.Play22(Namespaces.quote(form.service.namespace)),
      requestHolderClass = "play.api.libs.ws.WS.WSRequestHolder",
      authSchemeClass = "com.ning.http.client.Realm.AuthScheme",
      supportsHttpPatch = false
    )

    Play2ClientGenerator.invoke(config, form)
  }
}

object Play23ClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): String = {

    val config = PlayFrameworkVersion(
      name = "2.3.x",
      config = ScalaClientMethodConfigs.Play23(Namespaces.quote(form.service.namespace)),
      requestHolderClass = "play.api.libs.ws.WSRequestHolder",
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
  ): String = {
    Play2ClientGenerator(version, form).invoke()
  }

}

case class Play2ClientGenerator(
  version: PlayFrameworkVersion,
  form: InvocationForm
) {

  private val ssd = new ScalaService(form.service)

  def invoke(): String = {
    ApidocComments(form.service.version, form.userAgent).toJavaString + "\n" +
    Seq(
      Play2Models(form, addBindables = true, addHeader = false),
      client()
    ).mkString("\n\n")
  }

  private def client(): String = {

    val methodGenerator = ScalaClientMethodGenerator(version.config, ssd)

    val patchMethod = version.supportsHttpPatch match {
      case true => """_logRequest("PATCH", _requestHolder(path).withQueryString(queryParameters:_*)).patch(body.getOrElse(play.api.libs.json.Json.obj()))"""
      case false => s"""sys.error("PATCH method is not supported in Play Framework Version ${version.name}")"""
    }

    val headers = Headers(form)
    val headerString = headers.scala.
      map { case (name, value) => s""""$name" -> ${value}""" }.
      mkString(".withHeaders(\n        ", ",\n        ", "") + "\n      )"

    s"""package ${ssd.namespaces.base} {

${headers.objectConstants.indent(2)}

${ScalaClientCommon.clientSignature(version.config).indent(2)} {
    import ${ssd.namespaces.models}.json._

    private val logger = play.api.Logger("${ssd.namespaces.base}.Client")

    logger.info(s"Initializing ${ssd.namespaces.base}.Client for url $$apiUrl")

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    def _requestHolder(path: String): ${version.requestHolderClass} = {
      import play.api.Play.current

      val holder = play.api.libs.ws.WS.url(apiUrl + path)$headerString
      auth.fold(holder) { a =>
        a match {
          case Authorization.Basic(username, password) => {
            holder.withAuth(username, password.getOrElse(""), ${version.authSchemeClass}.BASIC)
          }
          case _ => sys.error("Invalid authorization scheme[" + a.getClass + "]")
        }
      }
    }

    def _logRequest(method: String, req: ${version.requestHolderClass})(implicit ec: scala.concurrent.ExecutionContext): ${version.requestHolderClass} = {
      val queryComponents = for {
        (name, values) <- req.queryString
        value <- values
      } yield name -> value
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
          _logRequest("POST", _requestHolder(path).withQueryString(queryParameters:_*)).post(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PUT" => {
          _logRequest("PUT", _requestHolder(path).withQueryString(queryParameters:_*)).put(body.getOrElse(play.api.libs.json.Json.obj()))
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
