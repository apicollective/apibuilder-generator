package scala.models.http4s.server

import scala.annotation.nowarn

import scala.generator.ScalaClientMethodConfigs

trait StatusCode {
  def code: Int
  def name: String
  def bodyType: Option[String] = None
  def withBodyType(@nowarn typ: String): StatusCode = this
  def responseParams(config: ScalaClientMethodConfigs.Http4s): String = (params(config).map { case (name, typ) => s"$name: $typ" } ++ Seq(s"headers: Seq[${config.headerRawClass}] = Nil")).mkString(", ")
  def responseExtractor(config: ScalaClientMethodConfigs.Http4s): String = (params(config).map(_._1) ++ Seq("headers")).mkString(", ")
  def applyArgs(config: ScalaClientMethodConfigs.Http4s): String = config match {
    case _: ScalaClientMethodConfigs.Http4s015 | _: ScalaClientMethodConfigs.Http4s017 =>
      params(config).map(_._1).mkString(", ") + ").putHeaders(headers: _*"
    case _ =>
      (params(config).map(_._1) ++ Seq("headers: _*")).mkString(", ")
  }
  protected def params(@nowarn config: ScalaClientMethodConfigs.Http4s): Seq[(String, String)] = bodyType.toSeq.map(typ => "value" -> typ)
}

case class EmptyStatusCode(code: Int, name: String) extends StatusCode

case class EntityStatusCode(code: Int, name: String, override val bodyType: Option[String] = None) extends StatusCode {
  override def withBodyType(typ: String) = this.copy(bodyType = Some(typ))
}

case class LocationStatusCode(code: Int, name: String) extends StatusCode {
  override def params(config: ScalaClientMethodConfigs.Http4s) = Seq("location" -> "org.http4s.Uri")
}

case class WWWAuthenticateStatusCode(code: Int, name: String, override val bodyType: Option[String] = None) extends StatusCode {
  override def withBodyType(typ: String) = this.copy(bodyType = Some(typ))
  override def params(config: ScalaClientMethodConfigs.Http4s) = super.params(config) ++ (config match {
    case _: ScalaClientMethodConfigs.Http4s015 | _: ScalaClientMethodConfigs.Http4s017 =>
      Seq("challenge" -> "org.http4s.Challenge", "challenges" -> "Seq[org.http4s.Challenge] = Nil")
    case _ =>
      Seq("authenticate" -> "org.http4s.headers.`WWW-Authenticate`")
  })
  override def applyArgs(config: ScalaClientMethodConfigs.Http4s) = config match {
    case _: ScalaClientMethodConfigs.Http4s015 | _: ScalaClientMethodConfigs.Http4s017 =>
      "challenge, challenges: _*).putHeaders(headers: _*"
    case _: ScalaClientMethodConfigs.Http4s018 | _: ScalaClientMethodConfigs.Http4s02xSeries =>
      bodyType.fold(
        "authenticate, headers: _*"
      )(_ =>
        "authenticate, value, headers: _*"
      )
  }
}

object HttpStatusCodes {
  private val all = Seq(
    EmptyStatusCode(100, "Continue"),
    EmptyStatusCode(101, "SwitchingProtocols"),
    EmptyStatusCode(102, "Processing"),
    EntityStatusCode(200, "Ok"),
    EntityStatusCode(201, "Created"),
    EntityStatusCode(202, "Accepted"),
    EntityStatusCode(203, "NonAuthoritativeInformation"),
    EmptyStatusCode(204, "NoContent"),
    EmptyStatusCode(205, "ResetContent"),
    EntityStatusCode(206, "PartialContent"),
    EntityStatusCode(207, "MultiStatus"),
    EntityStatusCode(208, "AlreadyReported"),
    EntityStatusCode(226, "IMUsed"),
    LocationStatusCode(300, "MultipleChoices"),
    LocationStatusCode(301, "MovedPermanently"),
    LocationStatusCode(302, "Found"),
    LocationStatusCode(303, "SeeOther"),
    EmptyStatusCode(304, "NotModified"),
    EmptyStatusCode(305, "UseProxy"),
    LocationStatusCode(307, "TemporaryRedirect"),
    LocationStatusCode(308, "PermanentRedirect"),
    EntityStatusCode(400, "BadRequest"),
    WWWAuthenticateStatusCode(401, "Unauthorized"),
    EntityStatusCode(402, "PaymentRequired"),
    EntityStatusCode(403, "Forbidden"),
    EntityStatusCode(404, "NotFound"),
    EntityStatusCode(405, "MethodNotAllowed"),
    EntityStatusCode(406, "NotAcceptable"),
    EntityStatusCode(407, "ProxyAuthenticationRequired"),
    EntityStatusCode(408, "RequestTimeout"),
    EntityStatusCode(409, "Conflict"),
    EntityStatusCode(410, "Gone"),
    EntityStatusCode(411, "LengthRequired"),
    EntityStatusCode(412, "PreconditionFailed"),
    EntityStatusCode(413, "PayloadTooLarge"),
    EntityStatusCode(414, "UriTooLong"),
    EntityStatusCode(415, "UnsupportedMediaType"),
    EntityStatusCode(416, "RangeNotSatisfiable"),
    EntityStatusCode(417, "ExpectationFailed"),
    EntityStatusCode(422, "UnprocessableEntity"),
    EntityStatusCode(423, "Locked"),
    EntityStatusCode(424, "FailedDependency"),
    EntityStatusCode(426, "UpgradeRequired"),
    EntityStatusCode(428, "PreconditionRequired"),
    EntityStatusCode(429, "TooManyRequests"),
    EntityStatusCode(431, "RequestHeaderFieldsTooLarge"),
    EntityStatusCode(451, "UnavailableForLegalReasons")
  ).map(s => s.code -> s).toMap

  def apply(code: Int): Option[StatusCode] = all.get(code)
}
