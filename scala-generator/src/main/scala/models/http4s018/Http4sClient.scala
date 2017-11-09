package models.http4s018

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
  import cats.effect.IO

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

    def modifyRequest(request: IO[org.http4s.Request[IO]]): IO[org.http4s.Request[IO]] = request

    implicit def circeJsonEncoder[A](implicit encoder: io.circe.Encoder[A]) = org.http4s.circe.jsonEncoderOf[IO, A]

    def _executeRequest[T, U](
      method: String,
      path: Seq[String],
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[T] = None
    )(handler: Response[IO] => IO[U]
    )(implicit encoder: io.circe.Encoder[T]): IO[U] = {
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

      val request = org.http4s.Request[IO](method = m,
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

      val authBody = body.fold(IO.pure(authReq))(authReq.withBody)

      asyncHttpClient.fetch(modifyRequest(authBody))(handler)
    }
  }

${Http4sScalaClientCommon(config).indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}
}"""
  }

}
