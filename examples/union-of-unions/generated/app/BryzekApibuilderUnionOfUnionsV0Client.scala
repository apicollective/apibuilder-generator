/**
 * Generated by API Builder - https://www.apibuilder.io
 * Service version: 0.9.61
 * apibuilder 0.15.33 localhost 9000/bryzek/apibuilder-union-of-unions/latest/play_2_6_client
 */
package com.bryzek.apibuilder.union.of.unions.v0.models {

  sealed trait Party extends _root_.scala.Product with _root_.scala.Serializable

  /**
   * Defines the valid discriminator values for the type Party
   */
  sealed trait PartyDiscriminator extends _root_.scala.Product with _root_.scala.Serializable

  object PartyDiscriminator {

    case object User extends PartyDiscriminator { override def toString = "user" }
    case object Group extends PartyDiscriminator { override def toString = "group" }

    final case class UNDEFINED(override val toString: String) extends PartyDiscriminator

    val all: scala.List[PartyDiscriminator] = scala.List(User, Group)

    private[this] val byName: Map[String, PartyDiscriminator] = all.map(x => x.toString.toLowerCase -> x).toMap

    def apply(value: String): PartyDiscriminator = fromString(value).getOrElse(UNDEFINED(value))

    def fromString(value: String): _root_.scala.Option[PartyDiscriminator] = byName.get(value.toLowerCase)

  }

  sealed trait User extends Party

  /**
   * Defines the valid discriminator values for the type User
   */
  sealed trait UserDiscriminator extends _root_.scala.Product with _root_.scala.Serializable

  object UserDiscriminator {

    case object RegisteredUser extends UserDiscriminator { override def toString = "registered_user" }
    case object GuestUser extends UserDiscriminator { override def toString = "guest_user" }

    final case class UNDEFINED(override val toString: String) extends UserDiscriminator

    val all: scala.List[UserDiscriminator] = scala.List(RegisteredUser, GuestUser)

    private[this] val byName: Map[String, UserDiscriminator] = all.map(x => x.toString.toLowerCase -> x).toMap

    def apply(value: String): UserDiscriminator = fromString(value).getOrElse(UNDEFINED(value))

    def fromString(value: String): _root_.scala.Option[UserDiscriminator] = byName.get(value.toLowerCase)

  }

  final case class Group(
    name: String
  ) extends Party

  final case class GuestUser(
    email: _root_.scala.Option[String] = None
  ) extends User

  /**
   * @param guid Internal unique identifier for this user.
   */
  final case class RegisteredUser(
    guid: _root_.java.util.UUID,
    email: String
  ) extends User

  /**
   * Provides future compatibility in clients - in the future, when a type is added
   * to the union Party, it will need to be handled in the client code. This
   * implementation will deserialize these future types as an instance of this class.
   *
   * @param description Information about the type that we received that is undefined in this version of
   *        the client.
   */
  final case class PartyUndefinedType(
    description: String
  ) extends Party

  /**
   * Provides future compatibility in clients - in the future, when a type is added
   * to the union User, it will need to be handled in the client code. This
   * implementation will deserialize these future types as an instance of this class.
   *
   * @param description Information about the type that we received that is undefined in this version of
   *        the client.
   */
  final case class UserUndefinedType(
    description: String
  ) extends User

}

package com.bryzek.apibuilder.union.of.unions.v0.models {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
    import com.bryzek.apibuilder.union.of.unions.v0.models.json._

    private[v0] implicit val jsonReadsUUID = __.read[String].map { str =>
      _root_.java.util.UUID.fromString(str)
    }

    private[v0] implicit val jsonWritesUUID = new Writes[_root_.java.util.UUID] {
      def writes(x: _root_.java.util.UUID) = JsString(x.toString)
    }

    private[v0] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
      _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(str)
    }

    private[v0] implicit val jsonWritesJodaDateTime = new Writes[_root_.org.joda.time.DateTime] {
      def writes(x: _root_.org.joda.time.DateTime) = {
        JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(x))
      }
    }

    private[v0] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
      _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate(str)
    }

    private[v0] implicit val jsonWritesJodaLocalDate = new Writes[_root_.org.joda.time.LocalDate] {
      def writes(x: _root_.org.joda.time.LocalDate) = {
        JsString(_root_.org.joda.time.format.ISODateTimeFormat.date.print(x))
      }
    }

    implicit def jsonReadsApibuilderUnionOfUnionsGroup: play.api.libs.json.Reads[Group] = {
      (__ \ "name").read[String].map { x => new Group(name = x) }
    }

    def jsObjectGroup(obj: com.bryzek.apibuilder.union.of.unions.v0.models.Group): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "name" -> play.api.libs.json.JsString(obj.name)
      ) ++ play.api.libs.json.Json.obj("discriminator" -> "group")
    }

    implicit def jsonWritesApibuilderUnionOfUnionsGroup: play.api.libs.json.Writes[Group] = {
      new play.api.libs.json.Writes[com.bryzek.apibuilder.union.of.unions.v0.models.Group] {
        def writes(obj: com.bryzek.apibuilder.union.of.unions.v0.models.Group) = {
          jsObjectGroup(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderUnionOfUnionsGuestUser: play.api.libs.json.Reads[GuestUser] = {
      (__ \ "email").readNullable[String].map { x => new GuestUser(email = x) }
    }

    def jsObjectGuestUser(obj: com.bryzek.apibuilder.union.of.unions.v0.models.GuestUser): play.api.libs.json.JsObject = {
      (obj.email match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("email" -> play.api.libs.json.JsString(x))
      }) ++ play.api.libs.json.Json.obj("discriminator" -> "guest_user")
    }

    implicit def jsonWritesApibuilderUnionOfUnionsGuestUser: play.api.libs.json.Writes[GuestUser] = {
      new play.api.libs.json.Writes[com.bryzek.apibuilder.union.of.unions.v0.models.GuestUser] {
        def writes(obj: com.bryzek.apibuilder.union.of.unions.v0.models.GuestUser) = {
          jsObjectGuestUser(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderUnionOfUnionsRegisteredUser: play.api.libs.json.Reads[RegisteredUser] = {
      for {
        guid <- (__ \ "guid").read[_root_.java.util.UUID]
        email <- (__ \ "email").read[String]
      } yield RegisteredUser(guid, email)
    }

    def jsObjectRegisteredUser(obj: com.bryzek.apibuilder.union.of.unions.v0.models.RegisteredUser): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "guid" -> play.api.libs.json.JsString(obj.guid.toString),
        "email" -> play.api.libs.json.JsString(obj.email)
      ) ++ play.api.libs.json.Json.obj("discriminator" -> "registered_user")
    }

    implicit def jsonWritesApibuilderUnionOfUnionsRegisteredUser: play.api.libs.json.Writes[RegisteredUser] = {
      new play.api.libs.json.Writes[com.bryzek.apibuilder.union.of.unions.v0.models.RegisteredUser] {
        def writes(obj: com.bryzek.apibuilder.union.of.unions.v0.models.RegisteredUser) = {
          jsObjectRegisteredUser(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderUnionOfUnionsParty: play.api.libs.json.Reads[Party] = new play.api.libs.json.Reads[Party] {
      def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[Party] = {
        println(s"Reading party: ${js}")
        (js \ "discriminator").asOpt[String].getOrElse { sys.error("Union[Party] requires a discriminator named 'discriminator' - this field was not found in the Json Value") } match {
          case "user" => js.validate[com.bryzek.apibuilder.union.of.unions.v0.models.User]
          case "group" => js.validate[com.bryzek.apibuilder.union.of.unions.v0.models.Group]
          case other => play.api.libs.json.JsSuccess(com.bryzek.apibuilder.union.of.unions.v0.models.PartyUndefinedType(other))
        }
      }
    }

    def jsObjectParty(obj: com.bryzek.apibuilder.union.of.unions.v0.models.Party): play.api.libs.json.JsObject = {
      obj match {
        case x: com.bryzek.apibuilder.union.of.unions.v0.models.User => jsObjectUser(x)
        case x: com.bryzek.apibuilder.union.of.unions.v0.models.Group => jsObjectGroup(x)
        case other => {
          sys.error(s"The type[${other.getClass.getName}] has no JSON writer")
        }
      }
    }

    implicit def jsonWritesApibuilderUnionOfUnionsParty: play.api.libs.json.Writes[Party] = {
      new play.api.libs.json.Writes[com.bryzek.apibuilder.union.of.unions.v0.models.Party] {
        def writes(obj: com.bryzek.apibuilder.union.of.unions.v0.models.Party) = {
          jsObjectParty(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderUnionOfUnionsUser: play.api.libs.json.Reads[User] = new play.api.libs.json.Reads[User] {
      def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[User] = {
        (js \ "discriminator").asOpt[String].getOrElse { sys.error("Union[User] requires a discriminator named 'discriminator' - this field was not found in the Json Value") } match {
          case "registered_user" => js.validate[com.bryzek.apibuilder.union.of.unions.v0.models.RegisteredUser]
          case "guest_user" => js.validate[com.bryzek.apibuilder.union.of.unions.v0.models.GuestUser]
          case other => play.api.libs.json.JsSuccess(com.bryzek.apibuilder.union.of.unions.v0.models.UserUndefinedType(other))
        }
      }
    }

    def jsObjectUser(obj: com.bryzek.apibuilder.union.of.unions.v0.models.User): play.api.libs.json.JsObject = {
      obj match {
        case x: com.bryzek.apibuilder.union.of.unions.v0.models.RegisteredUser => jsObjectRegisteredUser(x)
        case x: com.bryzek.apibuilder.union.of.unions.v0.models.GuestUser => jsObjectGuestUser(x)
        case other => {
          sys.error(s"The type[${other.getClass.getName}] has no JSON writer")
        }
      }
    }

    implicit def jsonWritesApibuilderUnionOfUnionsUser: play.api.libs.json.Writes[User] = {
      new play.api.libs.json.Writes[com.bryzek.apibuilder.union.of.unions.v0.models.User] {
        def writes(obj: com.bryzek.apibuilder.union.of.unions.v0.models.User) = {
          jsObjectUser(obj)
        }
      }
    }
  }
}

package com.bryzek.apibuilder.union.of.unions.v0 {

  object Bindables {

    import play.api.mvc.{PathBindable, QueryStringBindable}

    // import models directly for backwards compatibility with prior versions of the generator
    import Core._

    object Core {
      implicit def pathBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.org.joda.time.DateTime] = ApibuilderPathBindable(ApibuilderTypes.dateTimeIso8601)
      implicit def queryStringBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.org.joda.time.DateTime] = ApibuilderQueryStringBindable(ApibuilderTypes.dateTimeIso8601)

      implicit def pathBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.org.joda.time.LocalDate] = ApibuilderPathBindable(ApibuilderTypes.dateIso8601)
      implicit def queryStringBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.org.joda.time.LocalDate] = ApibuilderQueryStringBindable(ApibuilderTypes.dateIso8601)
    }

    trait ApibuilderTypeConverter[T] {

      def convert(value: String): T

      def convert(value: T): String

      def example: T

      def validValues: Seq[T] = Nil

      def errorMessage(key: String, value: String, ex: java.lang.Exception): String = {
        val base = s"Invalid value '$value' for parameter '$key'. "
        validValues.toList match {
          case Nil => base + "Ex: " + convert(example)
          case values => base + ". Valid values are: " + values.mkString("'", "', '", "'")
        }
      }
    }

    object ApibuilderTypes {
      val dateTimeIso8601: ApibuilderTypeConverter[_root_.org.joda.time.DateTime] = new ApibuilderTypeConverter[_root_.org.joda.time.DateTime] {
        override def convert(value: String): _root_.org.joda.time.DateTime = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(value)
        override def convert(value: _root_.org.joda.time.DateTime): String = _root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(value)
        override def example: _root_.org.joda.time.DateTime = _root_.org.joda.time.DateTime.now
      }

      val dateIso8601: ApibuilderTypeConverter[_root_.org.joda.time.LocalDate] = new ApibuilderTypeConverter[_root_.org.joda.time.LocalDate] {
        override def convert(value: String): _root_.org.joda.time.LocalDate = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate(value)
        override def convert(value: _root_.org.joda.time.LocalDate): String = _root_.org.joda.time.format.ISODateTimeFormat.date.print(value)
        override def example: _root_.org.joda.time.LocalDate = _root_.org.joda.time.LocalDate.now
      }
    }

    final case class ApibuilderQueryStringBindable[T](
      converters: ApibuilderTypeConverter[T]
    ) extends QueryStringBindable[T] {

      override def bind(key: String, params: Map[String, Seq[String]]): _root_.scala.Option[_root_.scala.Either[String, T]] = {
        params.getOrElse(key, Nil).headOption.map { v =>
          try {
            Right(
              converters.convert(v)
            )
          } catch {
            case ex: java.lang.Exception => Left(
              converters.errorMessage(key, v, ex)
            )
          }
        }
      }

      override def unbind(key: String, value: T): String = {
        s"$key=${converters.convert(value)}"
      }
    }

    final case class ApibuilderPathBindable[T](
      converters: ApibuilderTypeConverter[T]
    ) extends PathBindable[T] {

      override def bind(key: String, value: String): _root_.scala.Either[String, T] = {
        try {
          Right(
            converters.convert(value)
          )
        } catch {
          case ex: java.lang.Exception => Left(
            converters.errorMessage(key, value, ex)
          )
        }
      }

      override def unbind(key: String, value: T): String = {
        converters.convert(value)
      }
    }

  }

}


package com.bryzek.apibuilder.union.of.unions.v0 {

  object Constants {

    val Namespace = "com.bryzek.apibuilder.union.of.unions.v0"
    val UserAgent = "apibuilder 0.15.33 localhost 9000/bryzek/apibuilder-union-of-unions/latest/play_2_6_client"
    val Version = "0.9.61"
    val VersionMajor = 0

  }

  class Client(
    ws: play.api.libs.ws.WSClient,
    val baseUrl: String,
    auth: scala.Option[com.bryzek.apibuilder.union.of.unions.v0.Authorization] = None,
    defaultHeaders: Seq[(String, String)] = Nil
  ) extends interfaces.Client {
    import com.bryzek.apibuilder.union.of.unions.v0.models.json._

    private[this] val logger = play.api.Logger("com.bryzek.apibuilder.union.of.unions.v0.Client")

    logger.info(s"Initializing com.bryzek.apibuilder.union.of.unions.v0.Client for url $baseUrl")





    def _requestHolder(path: String): play.api.libs.ws.WSRequest = {

      val holder = ws.url(baseUrl + path).addHttpHeaders(
        "User-Agent" -> Constants.UserAgent,
        "X-Apidoc-Version" -> Constants.Version,
        "X-Apidoc-Version-Major" -> Constants.VersionMajor.toString
      ).addHttpHeaders(defaultHeaders : _*)
      auth.fold(holder) {
        case Authorization.Basic(username, password) => {
          holder.withAuth(username, password.getOrElse(""), play.api.libs.ws.WSAuthScheme.BASIC)
        }
        case a => sys.error("Invalid authorization scheme[" + a.getClass + "]")
      }
    }

    def _logRequest(method: String, req: play.api.libs.ws.WSRequest): play.api.libs.ws.WSRequest = {
      val queryComponents = for {
        (name, values) <- req.queryString
        value <- values
      } yield s"$name=$value"
      val url = s"${req.url}${queryComponents.mkString("?", "&", "")}"
      auth.fold(logger.info(s"curl -X $method '$url'")) { _ =>
        logger.info(s"curl -X $method -u '[REDACTED]:' '$url'")
      }
      req
    }

    def _executeRequest(
      method: String,
      path: String,
      queryParameters: Seq[(String, String)] = Nil,
      requestHeaders: Seq[(String, String)] = Nil,
      body: Option[play.api.libs.json.JsValue] = None
    ): scala.concurrent.Future[play.api.libs.ws.WSResponse] = {
      method.toUpperCase match {
        case "GET" => {
          _logRequest("GET", _requestHolder(path).addHttpHeaders(requestHeaders:_*).addQueryStringParameters(queryParameters:_*)).get()
        }
        case "POST" => {
          _logRequest("POST", _requestHolder(path).addHttpHeaders(_withJsonContentType(requestHeaders):_*).addQueryStringParameters(queryParameters:_*)).post(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PUT" => {
          _logRequest("PUT", _requestHolder(path).addHttpHeaders(_withJsonContentType(requestHeaders):_*).addQueryStringParameters(queryParameters:_*)).put(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "PATCH" => {
          _logRequest("PATCH", _requestHolder(path).addHttpHeaders(requestHeaders:_*).addQueryStringParameters(queryParameters:_*)).patch(body.getOrElse(play.api.libs.json.Json.obj()))
        }
        case "DELETE" => {
          _logRequest("DELETE", _requestHolder(path).addHttpHeaders(requestHeaders:_*).addQueryStringParameters(queryParameters:_*)).delete()
        }
         case "HEAD" => {
          _logRequest("HEAD", _requestHolder(path).addHttpHeaders(requestHeaders:_*).addQueryStringParameters(queryParameters:_*)).head()
        }
         case "OPTIONS" => {
          _logRequest("OPTIONS", _requestHolder(path).addHttpHeaders(requestHeaders:_*).addQueryStringParameters(queryParameters:_*)).options()
        }
        case _ => {
          _logRequest(method, _requestHolder(path).addHttpHeaders(requestHeaders:_*).addQueryStringParameters(queryParameters:_*))
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

  object Client {

    def parseJson[T](
      className: String,
      r: play.api.libs.ws.WSResponse,
      f: (play.api.libs.json.JsValue => play.api.libs.json.JsResult[T])
    ): T = {
      f(play.api.libs.json.Json.parse(r.body)) match {
        case play.api.libs.json.JsSuccess(x, _) => x
        case play.api.libs.json.JsError(errors) => {
          throw com.bryzek.apibuilder.union.of.unions.v0.errors.FailedRequest(r.status, s"Invalid json for class[" + className + "]: " + errors.mkString(" "))
        }
      }
    }

  }

  sealed trait Authorization extends _root_.scala.Product with _root_.scala.Serializable
  object Authorization {
    final case class Basic(username: String, password: Option[String] = None) extends Authorization
  }

  package interfaces {

    trait Client {
      def baseUrl: String

    }

  }



  package errors {

    final case class FailedRequest(responseCode: Int, message: String, requestUri: Option[_root_.java.net.URI] = None) extends _root_.java.lang.Exception(s"HTTP $responseCode: $message")

  }

}