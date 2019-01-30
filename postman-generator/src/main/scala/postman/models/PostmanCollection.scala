package postman.models

import java.util.UUID
import play.api.libs.json._

/**
  * Postman Collection model draft
  * based on:
  * https://schema.getpostman.com/json/collection/v2.1.0/docs/index.html
  *
  * Contains several shortcuts
  */
case class PostmanCollection(
  info: PostmanCollectionInfo,
  item: Seq[PostmanCollectionEntry],
  variable: Seq[PostmanVariable],
  auth: Option[Auth]
)

case class PostmanCollectionInfo(
  name: String,
  `_postman_id`: Option[String],
  description: Option[String],
  schema: String = "https://schema.getpostman.com/json/collection/v2.1.0/collection.json"
)

sealed trait PostmanCollectionEntry

case class PostmanCollectionFolder(
  name: String,
  description: Option[String],
  item: Seq[PostmanCollectionItem]
) extends PostmanCollectionEntry

case class PostmanCollectionItem(
  id: Option[String],
  name: Option[String],
  description: Option[String],
  request: PostmanRequest,
  response: Seq[PostmanCollectionItemResponse] = Seq.empty,
  event: Seq[Event] = Seq.empty,
) extends PostmanCollectionEntry

case class PostmanRequest(
  url: PostmanRequestUrl,
  method: String,
  description: Option[String] = None,
  headers: Seq[PostmanHeader] = Seq.empty,
  body: Option[PostmanRequestBody] = None
)

case class PostmanRequestUrl(
  raw: String,
  protocol: String,
  host: Seq[String] = Seq.empty,
  path: Seq[String] = Seq.empty,
  query: Seq[PostmanUrlQuery] = Seq.empty,
  variable: Seq[PostmanUrlPathVariable] = Seq.empty
)

case class PostmanUrlQuery(
  key: String,
  value: Option[String],
  description: Option[String],
  disabled: Boolean
)

case class PostmanUrlPathVariable(
  key: String,
  value: Option[String],
  description: Option[String],
  disabled: Boolean
)

object PostmanHeader {
  def apply(key: String, value: String): PostmanHeader =
    PostmanHeader(key, Some(value), None)
}

case class PostmanHeader(
  key: String,
  value: Option[String],
  description: Option[String]
)

sealed trait PostmanRequestBody {
  val mode: String
  val raw: String
}

case class PostmanRequestBodyRaw(
  raw: String
) extends PostmanRequestBody {
  override val mode = "raw"
}

case class PostmanCollectionItemResponse(
  id: Option[String],
  name: Option[String],
  originalRequest: Option[PostmanRequest],
  header: Seq[PostmanHeader],
  body: Option[String],
  `_postman_previewlanguage`: Option[String],
  status: Option[String],
  code: Int
)

case class PostmanVariable(
  key: String,
  value: String,
  `type`: String = "string"
)

object AuthType extends Enumeration {
  type AuthType = Value
  val awsv4, basic, bearer, digest, hawk, noauth, oauth1, oauth2, ntlm = Value

  implicit val reads = Reads.enumNameReads(AuthType)
  implicit val writes = Writes.enumNameWrites
}

case class AuthEntry(
  key: String,
  value: String,
  `type`: String = "string"
)

case class Auth(
  `type`: AuthType.AuthType,
  basic: Seq[AuthEntry]
)

object EventType extends Enumeration {
  type EventType = Value
  val test, prerequest = Value

  implicit val reads = Reads.enumNameReads(EventType)
  implicit val writes = Writes.enumNameWrites
}

case class Event(
  //  id: String = UUID.randomUUID().toString,
  listen: EventType.EventType,
  script: Script
)

case class Script(
  exec: Seq[String],
  id: String = UUID.randomUUID().toString,
  `type`: String = "'text/javascript"
)


object json {

  import AuthType._
  import EventType._

  implicit val header = Json.writes[PostmanHeader]
  implicit val requestBody = new Writes[PostmanRequestBody] {
    override def writes(o: PostmanRequestBody): JsValue = Json.obj("mode" -> o.mode, "raw" -> o.raw)
  }
  implicit val urlQuery = Json.writes[PostmanUrlQuery]
  implicit val urlPath = Json.writes[PostmanUrlPathVariable]
  implicit val url = Json.writes[PostmanRequestUrl]

  implicit val script = Json.writes[Script]
  implicit val event = Json.writes[Event]
  implicit val request = Json.writes[PostmanRequest]
  implicit val response = Json.writes[PostmanCollectionItemResponse]
  implicit val collectionItem = Json.writes[PostmanCollectionItem]
  implicit val collectionInfo = Json.writes[PostmanCollectionInfo]
  implicit val folder = Json.writes[PostmanCollectionFolder]
  implicit val collectionEntry = Json.writes[PostmanCollectionEntry]
  implicit val variable = Json.writes[PostmanVariable]
  implicit val authEntry = Json.writes[AuthEntry]
  implicit val auth = Json.writes[Auth]
  implicit val collection = Json.writes[PostmanCollection]
}
