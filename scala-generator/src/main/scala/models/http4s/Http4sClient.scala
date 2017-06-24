package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import scala.generator.{ScalaClientMethodConfig, ScalaUtil}
import scala.models.{Headers, JsonImports}

import lib.Text._


case class Http4sClient(
  form: InvocationForm,
  ssd: ScalaService,
  config: ScalaClientMethodConfig
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
    implicit def circeJsonDecoder[A](implicit decoder: io.circe.Decoder[A]) = org.http4s.circe.jsonOf[A]
    implicit def circeJsonEncoder[A](implicit encoder: io.circe.Encoder[A]) = org.http4s.circe.jsonEncoderOf[A]
    import scalaz._
    import scalaz.concurrent.Task
${JsonImports(form.service).mkString("\n").indent(4)}

    def closeAsyncHttpClient() {
      asyncHttpClient.shutdownNow()
    }

${methodGenerator.accessors().indent(4)}

${methodGenerator.objects().indent(4)}

    private lazy val defaultApiHeaders = Seq(
${headerString.indent(6)}
    )

    def apiHeaders: Seq[(String, String)] = defaultApiHeaders

    def _executeRequest[T, U](
      method: String,
      path: Seq[String],
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[T] = None
    )(handler: Response => Task[U]
    )(implicit encoder: org.http4s.EntityEncoder[T]): Task[U] = {
      import org.http4s.QueryParamEncoder._

      val m = org.http4s.Method.fromString(method) match {
        case \\/-(m) => m
        case -\\/(e) => sys.error(e.toString)
      }

      val headers = org.http4s.Headers((
        apiHeaders ++
        defaultHeaders ++
        requestHeaders
      ).toList.map { case (k, v) => org.http4s.Header(k, v) })

      val queryMap = queryParameters.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      val uri = path.foldLeft(baseUrl){ case (uri, segment) => uri / segment }.setQueryParams(queryMap)

      val request = org.http4s.Request(method = m,
                                       uri = uri,
                                       headers = headers)

      val authReq = auth.fold(request) {
        case Authorization.Basic(username, passwordOpt) => {
          request.putHeaders(org.http4s.Header("Authorization", org.http4s.BasicCredentials(username, passwordOpt.getOrElse("")).value))
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }

      val authBody = body.fold(Task.now(authReq))(authReq.withBody)

      asyncHttpClient.fetch(authBody)(handler)
    }
  }

${Http4sScalaClientCommon(config).indent(2)}

${methodGenerator.traitsAndErrors().indent(2)}
}"""
  }

}
