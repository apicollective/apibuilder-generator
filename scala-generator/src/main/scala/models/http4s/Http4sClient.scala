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

${headers.objectConstants.indentString(2)}

${Http4sScalaClientCommon.clientSignature(config).indentString(2)} {
    import org.http4s.Response
${JsonImports(form.service).mkString("\n").indentString(4)}
${config.closeClient.getOrElse("")}

${methodGenerator.accessors().indentString(4)}

${methodGenerator.objects().indentString(4)}

    private lazy val defaultApiHeaders = Seq(
${headerString.indentString(6)}
    )

    def apiHeaders: Seq[(String, String)] = defaultApiHeaders

    def modifyRequest(request: ${config.requestType}): ${config.requestType} = request

    implicit def circeJsonEncoder[${config.asyncTypeParam(Some(config.asyncTypeConstraint)).map(p => p + ", ").getOrElse("")}A](implicit encoder: io.circe.Encoder[A]) = ${config.generateCirceJsonEncoderOf("A")}

    def _executeRequest[T, U](
      method: String,
      path: Seq[String],
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[T] = None,
      formBody : Option[org.http4s.UrlForm] = None
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
      ).groupBy(_._1).map { case (k, l) => ${config.headerConstructor("k","l.last._2")} }.toList)

      val queryMap = queryParameters.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      val uri = path.foldLeft(baseUrl){ case (uri, segment) => uri / segment }.setQueryParams(queryMap)

      val request = ${config.requestClass}(method = m,
                                       uri = uri,
                                       headers = headers)

      val reqAndMaybeAuth = auth.fold(request) {
        case Authorization.Basic(username, passwordOpt) => {
          val userpass = s"$$username:$${passwordOpt.getOrElse("")}"
          val token = java.util.Base64.getEncoder.encodeToString(userpass.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1))
          request.putHeaders(${config.headerConstructor("\"Authorization\"","s\"Basic $token\"")})
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }

${config.reqAndMaybeAuthAndBody.indentString(6)}

      ${config.httpClient}.fetch(modifyRequest(reqAndMaybeAuthAndBody))(handler)
    }
  }

${Http4sScalaClientCommon(config).indentString(2)}

${methodGenerator.traitsAndErrors().indentString(2)}
}"""
  }

}
