package models.attributes

import _root_.play.api.libs.json.Json

object ObjectReferenceAttribute {

  val Key = "object-reference"

  case class ObjectReferenceAttrValue(
    relatedServiceNamespace: String,
    resourceType: String,
    operationMethod: String,
    identifierField: String) {
    def toPostmanVariableName: String = s"$resourceType#$identifierField"
    def toPostmanVariableRef: String = s"{{$toPostmanVariableName}}"
  }

  implicit val objectReferenceAttrValueFormats = Json.format[ObjectReferenceAttrValue]

}
