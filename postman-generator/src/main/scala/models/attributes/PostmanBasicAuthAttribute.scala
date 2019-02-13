package models.attributes

import play.api.libs.json.Json

object PostmanBasicAuthAttribute {

  val Key = "postman-basic-auth"

  case class PostmanBasicAuthAttrValue(
    username: String,
    password: String
  )

  implicit val postmanBasicAuthAttrValueFormat = Json.format[PostmanBasicAuthAttrValue]
}
