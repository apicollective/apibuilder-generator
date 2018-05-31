package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import scala.generator.{ScalaClientMethodConfigs, ScalaUtil}
import scala.models.{Headers, JsonImports}

import lib.Text._


case class Http4sClient(
  form: InvocationForm,
  ssd: ScalaService,
  config: ScalaClientMethodConfigs.Http4s
) {

  def generate(): String = {
    val headers = Headers(form)
    val headerString = headers.scala.map { case (name, value) =>
      s"(${ScalaUtil.wrapInQuotes(name)}, $value)"
    }.mkString(",\n")

    val methodGenerator = new ScalaClientMethodGenerator(config, ssd)

    s"""package ${ssd.namespaces.base} {
${config.clientImports}

${headers.objectConstants.indent(2)}

${Http4sScalaClientCommon.clientSignature(config).indent(2)} {
    import org.http4s.Response
${JsonImports(form.service).mkString("\n").indent(4)}
${config.closeClient.getOrElse("")}

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    private lazy val defaultApiHeaders = Seq(
${headerString.indent(6)}
    )

    def apiHeaders: Seq[(String, String)] = defaultApiHeaders

    def modifyRequest(request: ${config.asyncType}[${config.requestBuilderClass}]): ${config.asyncType}[${config.requestBuilderClass}] = request

    implicit def circeJsonEncoder[${config.asyncTypeParam(Some("Sync")).map(p => p + ", ").getOrElse("")}A](implicit encoder: io.circe.Encoder[A]) = ${config.generateCirceJsonEncoderOf("A")}

    def _executeRequest[T, U](
      method: String,
      path: Seq[String],
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[T] = None,
      formBody: Option[org.http4s.UrlForm] = None,
      requestModifier: ${config.requestBuilderClass} => ${config.requestBuilderClass} = identity
    )(handler: ${config.responseClass} => ${config.asyncType}[U]
    )(implicit encoder: io.circe.Encoder[T]): ${config.asyncType}[U] = {
      import org.http4s.QueryParamEncoder._

      val m = org.http4s.Method.fromString(method) match {
        case ${config.rightType}(m) => m
        case ${config.leftType}(e) => sys.error(e.toString)
      }

      val headers = org.http4s.Headers((
        apiHeaders ++
        defaultHeaders ++
        requestHeaders
      ).toList.map { case (k, v) => org.http4s.Header(k, v) })

      val queryMap = queryParameters.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      val uri = path.foldLeft(baseUrl){ case (uri, segment) => uri / segment }.setQueryParams(queryMap)

      val request = ${config.requestBuilderClass}(method = m, uri = uri, headers = headers)

      val reqAndMaybeAuth = auth.fold(request) {
        case Authorization.Basic(username, passwordOpt) => {
          val userpass = s"$$username:$${passwordOpt.getOrElse("")}"
          val token = java.util.Base64.getEncoder.encodeToString(userpass.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1))
          request.putHeaders(org.http4s.Header("Authorization", s"Basic $$token"))
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }

      val decoratedRequest = requestModifier(reqAndMaybeAuth)

      val reqAndMaybeAuthAndBody = if (formBody.nonEmpty) {
        formBody.fold(Sync[F].pure(decoratedRequest))(decoratedRequest.withBody)
      } else body.fold(Sync[F].pure(decoratedRequest))(decoratedRequest.withBody)

      ${config.httpClient}.fetch(modifyRequest(reqAndMaybeAuthAndBody))(handler)
    }${methodGenerator.modelErrors().indent(4)}
  }

${Http4sScalaClientCommon(config).indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}
}"""
  }

}
