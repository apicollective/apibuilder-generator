package scala.models

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator

import scala.generator._

case class PlayFrameworkVersion(
  name: String,
  config: ScalaClientMethodConfig,
  requestHolderClass: String,
  authSchemeClass: String,
  supportsHttpPatch: Boolean,
  useSpecificAddMethods: Boolean,
)

object Play22ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.2.x",
    config = ScalaClientMethodConfigs.Play22(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WS.WSRequestHolder",
    authSchemeClass = "com.ning.http.client.Realm.AuthScheme",
    supportsHttpPatch = false,
    useSpecificAddMethods = false,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play23ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.3.x",
    config = ScalaClientMethodConfigs.Play23(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequestHolder",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = false,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play24ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.4.x",
    config = ScalaClientMethodConfigs.Play24(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes),form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequest",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = false,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play25ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.5.x",
    config = ScalaClientMethodConfigs.Play25(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequest",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = false,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play26ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.6.x",
    config = ScalaClientMethodConfigs.Play26(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequest",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = true,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play26EnvelopeClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.6.x",
    config = ScalaClientMethodConfigs.Play26Envelope(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequest",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = true,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play27ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.7.x",
    config = ScalaClientMethodConfigs.Play27(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequest",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = true,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

}

object Play28ClientGenerator extends CodeGenerator {

  def config(form: InvocationForm) = PlayFrameworkVersion(
    name = "2.8.x",
    config = ScalaClientMethodConfigs.Play28(Namespaces.quote(form.service.namespace), Attributes.PlayDefaultConfig.withAttributes(form.attributes), form.service.baseUrl),
    requestHolderClass = "play.api.libs.ws.WSRequest",
    authSchemeClass = "play.api.libs.ws.WSAuthScheme",
    supportsHttpPatch = true,
    useSpecificAddMethods = true,
  )

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] =
    Play2ClientGenerator.invoke(config(form), form)

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
  form: InvocationForm,
  defaultAttributes: Attributes = Attributes.PlayDefaultConfig
) {

  private[this] val attributes = defaultAttributes.withAttributes(form.attributes)
  private[this] val ssd = new ScalaService(form.service, attributes)

  def invoke(): Either[Seq[String], Seq[File]] = {
    Right(generateCode())
  }

  private def generateCode(): Seq[File] = {
    val source = ApidocComments(form.service.version, form.userAgent).toJavaString + "\n" +
      Seq(
        Play2Models.generateCode(form, addBindables = true, addHeader = false, useBuiltInImplicits = false).map(_.contents).mkString("\n\n"),
        client()
      ).mkString("\n\n")

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", source, Some("Scala")))
  }

  def client(): String = {

    val methodGenerator = new ScalaClientMethodGenerator(version.config, ssd)

    val (addHeadersMethod, addQueryStringMethod) = version.useSpecificAddMethods match {
      case true => ("addHttpHeaders", "addQueryStringParameters")
      case false => ("withHeaders", "withQueryString")
    }

    val patchMethod = version.supportsHttpPatch match {
      case true => s"""_logRequest("PATCH", _requestHolder(path).$addHeadersMethod(requestHeaders:_*).$addQueryStringMethod(queryParameters:_*)).patch(body.getOrElse(play.api.libs.json.Json.obj()))"""
      case false => s"""sys.error("PATCH method is not supported in Play Framework Version ${version.name}")"""
    }

    val headers = Headers(form)
    val headerString = headers.scala.
      map { case (name, value) => s""""$name" -> ${value}""" }.
      mkString(s".$addHeadersMethod(\n        ", ",\n        ", "") + s"\n      ).$addHeadersMethod(defaultHeaders : _*)"
    val responseEnvelopeString = version.config.responseEnvelopeClassName match {
      case None => ""
      case Some(name) => PlayScalaClientCommon.responseEnvelopeTrait(name).indentString() + "\n\n"
    }

    s"""package ${ssd.namespaces.base} {

${headers.objectConstants.indentString(2)}

$responseEnvelopeString${PlayScalaClientCommon.clientSignature(version.config).indentString(2)} {
${JsonImports(form.service).mkString("\n").indentString(4)}

    private[this] val logger = play.api.Logger("${ssd.namespaces.base}.Client")

    logger.info(s"Initializing ${ssd.namespaces.base}.Client for url $$baseUrl")

${methodGenerator.accessors().indentString(4)}

${methodGenerator.objects().indentString(4)}

    def _requestHolder(path: String): ${version.requestHolderClass} = {
${if (version.config.expectsInjectedWsClient) "" else "      import play.api.Play.current\n"}
      val holder = ${if (version.config.expectsInjectedWsClient) "ws" else "play.api.libs.ws.WS"}.url(baseUrl + path)$headerString
      auth.fold(holder) {
        case Authorization.Basic(username, password) => {
          holder.withAuth(username, password.getOrElse(""), ${version.authSchemeClass}.BASIC)
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }
    }

    def _logRequest(method: String, req: ${version.requestHolderClass}): ${version.requestHolderClass} = {
      val queryComponents = for {
        (name, values) <- req.queryString
        value <- values
      } yield s"$$name=$$value"
      val url = s"$${req.url}$${queryComponents.mkString("?", "&", "")}"
      auth.fold(logger.info(s"curl -X $$method '$$url'")) { _ =>
        logger.info(s"curl -X $$method -u '[REDACTED]:' '$$url'")
      }
      req
    }

    def _executeRequest(
      method: String,
      path: String,
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[play.api.libs.json.JsValue] = None
    ): scala.concurrent.Future[${version.config.responseClass}] = {
      method.toUpperCase match {
        case "GET" => {
          _logRequest("GET", _requestHolder(path).$addHeadersMethod(requestHeaders:_*).$addQueryStringMethod(queryParameters:_*)).get()
        }
        case "POST" => {
          _logRequest("POST", _requestHolder(path).$addHeadersMethod(_withJsonContentType(requestHeaders):_*).$addQueryStringMethod(queryParameters:_*)).post(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PUT" => {
          _logRequest("PUT", _requestHolder(path).$addHeadersMethod(_withJsonContentType(requestHeaders):_*).$addQueryStringMethod(queryParameters:_*)).put(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PATCH" => {
          $patchMethod
        }
        case "DELETE" => {
          _logRequest("DELETE", _requestHolder(path).$addHeadersMethod(requestHeaders:_*).$addQueryStringMethod(queryParameters:_*)).delete()
        }
         case "HEAD" => {
          _logRequest("HEAD", _requestHolder(path).$addHeadersMethod(requestHeaders:_*).$addQueryStringMethod(queryParameters:_*)).head()
        }
         case "OPTIONS" => {
          _logRequest("OPTIONS", _requestHolder(path).$addHeadersMethod(requestHeaders:_*).$addQueryStringMethod(queryParameters:_*)).options()
        }
        case _ => {
          _logRequest(method, _requestHolder(path).$addHeadersMethod(requestHeaders:_*).$addQueryStringMethod(queryParameters:_*))
          sys.error("Unsupported method[%s]".format(method))
        }
      }
    }

    /**
     * Adds a Content-Type: application/json header unless the specified requestHeaders
     * already contain a Content-Type header
     */
    def _withJsonContentType(headers: Seq[(String, String)]): Seq[(String, String)] = {
      headers.find { _._1.toUpperCase == "CONTENT-TYPE" } match {
        case None => headers ++ Seq(("Content-Type" -> "application/json; charset=UTF-8"))
        case Some(_) => headers
      }
    }

  }

${PlayScalaClientCommon(version.config).indentString(2)}

${methodGenerator.traitsAndErrors().indentString(2)}

}"""
  }

}
