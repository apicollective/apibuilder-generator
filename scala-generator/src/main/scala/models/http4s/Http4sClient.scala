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
  import org.http4s.client.blaze._

${headers.objectConstants.indent(2)}

${Http4sScalaClientCommon.clientSignature(config).indent(2)} {
    import org.http4s.Response
    import ${config.asyncType}
${JsonImports(form.service).mkString("\n").indent(4)}

    def closeAsyncHttpClient(): Unit = {
      asyncHttpClient.shutdownNow()
    }

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    private lazy val defaultApiHeaders = Seq(
${headerString.indent(6)}
    )

    def apiHeaders: Seq[(String, String)] = defaultApiHeaders

    def modifyRequest(request: ${config.asyncType}[${config.requestClass}]): ${config.asyncType}[${config.requestClass}] = request

    implicit def circeJsonEncoder[A](implicit encoder: io.circe.Encoder[A]) = ${config.generateCirceJsonEncoderOf("A")}

    def _executeRequest[T, U](
      method: String,
      path: Seq[String],
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[T] = None
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
      val uri = path.foldLeft(baseUrl){ case (uri, segment) => uri / segment.takeWhile(_ != '#') }.setQueryParams(queryMap)

      val request = ${config.requestClass}(method = m,
                                       uri = uri,
                                       headers = headers)

      val authReq = auth.fold(request) {
        case Authorization.Basic(username, passwordOpt) => {
          val userpass = s"$$username:$${passwordOpt.getOrElse("")}"
          val token = java.util.Base64.getEncoder.encodeToString(userpass.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1))
          request.putHeaders(org.http4s.Header("Authorization", s"Basic $$token"))
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }

      val authBody = body.fold(${config.asyncType}.${config.asyncSuccess}(authReq))(authReq.withBody)

      asyncHttpClient.fetch(modifyRequest(authBody))(handler)
    }
  }

${Http4sScalaClientCommon(config).indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}
}"""
  }

}
