package models.attributes

import io.flow.postman.generator.attributes.v0.models._
import play.api.libs.json._
import io.flow.postman.generator.attributes.v0.models.json._
import io.apibuilder.spec.v0.models.json._
import play.api.libs.json.JsonNaming.SnakeCase

import scala.language.implicitConversions

object PostmanAttributes {

  /**
    * Allows to reason and operate on path and model references together.
    */
  case class ExtendedObjectReference(
    relatedServiceNamespace: String,
    resourceType: String,
    operationMethod: io.apibuilder.spec.v0.models.Method,
    identifierField: String,
    path: Option[String] = None
  )

  implicit class ModelReferenceExtend(val m: ModelReference) extends AnyVal {

    def toExtended: ExtendedObjectReference = ExtendedObjectReference(
      relatedServiceNamespace = m.relatedServiceNamespace,
      resourceType = m.resourceType,
      operationMethod = m.operationMethod,
      identifierField = m.identifierField
    )
  }

  implicit class PathReferenceExtend(val p: PathReference) extends AnyVal {

    def toExtended: ExtendedObjectReference = ExtendedObjectReference(
        relatedServiceNamespace = p.relatedServiceNamespace,
        resourceType = p.resourceType,
        operationMethod = p.operationMethod,
        identifierField = p.identifierField,
        path = p.path
      )
  }

  def postmanVariableNameFrom(objReference: ExtendedObjectReference): String = {
    import objReference._
    s"$resourceType#$identifierField"
  }

  def postmanVariableRefFrom(objectReference: ExtendedObjectReference): String = {
    val variableName = postmanVariableNameFrom(objectReference)
    s"{{$variableName}}"
  }

  final case class PostmanBasicAuthAttrValue(
    username: String,
    password: String
  )

  implicit val config = JsonConfiguration(SnakeCase)
  implicit val ReferenceAttrValueFormats = Json.format[ExtendedObjectReference]
}
