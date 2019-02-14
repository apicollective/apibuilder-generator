package models.attributes

import play.api.libs.json.Json

object PostmanAttributes {

  val BasicAuthKey = "postman-basic-auth"
  val SetupKey = "postman-organization-setup"
  val ObjectReferenceKey = "object-reference"

  case class PostmanBasicAuthAttrValue(
    username: String,
    password: String
  )

  case class ObjectReferenceAttrValue(
    relatedServiceNamespace: String,
    resourceType: String,
    operationMethod: String,
    identifierField: String) {
    def toPostmanVariableName: String = s"$resourceType#$identifierField"
    def toPostmanVariableRef: String = s"{{$toPostmanVariableName}}"
  }

  implicit val objectReferenceAttrValueFormats = Json.format[ObjectReferenceAttrValue]
  implicit val postmanBasicAuthAttrValueFormat = Json.format[PostmanBasicAuthAttrValue]
}
