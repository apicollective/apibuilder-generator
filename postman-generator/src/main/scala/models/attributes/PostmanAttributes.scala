package models.attributes

import io.apibuilder.spec.v0.models.Parameter
import io.postman.generator.attributes.v0.models.ObjectReference

import scala.language.implicitConversions

object PostmanAttributes {

  /**
    * This class basically extends the auto-generated [[ObjectReference]]
    * it contains one additional field with Postman variable name created by the generator
    *
    * As original auto-generated [[ObjectReference]] class is 'final case class'
    * we need to keep [[ExtendedObjectReference]] up to date manually, because we can't 'extend' the parent for real.
    */
  case class ExtendedObjectReference(
    relatedServiceNamespace: String,
    resourceType: String,
    operationMethod: io.apibuilder.spec.v0.models.Method,
    operationPath: String,
    identifierField: String,
    queryParams: Option[Map[String, String]],
    deleteOperationPath: Option[String],
    postmanVariableName: PostmanVariableName
  )

  case class PostmanVariableName(name: String) {
    def reference: String = s"{{$name}}"
  }

  implicit class ObjectReferenceExtend(val objRef: ObjectReference) extends AnyVal {

    def toExtended: ExtendedObjectReference = ExtendedObjectReference(
      relatedServiceNamespace = objRef.relatedServiceNamespace,
      resourceType = objRef.resourceType,
      operationMethod = objRef.operationMethod,
      operationPath = objRef.operationPath,
      identifierField = objRef.identifierField,
      queryParams = objRef.queryParams,
      deleteOperationPath = objRef.deleteOperationPath,
      postmanVariableName = postmanVariableNameFrom(objRef)
    )
  }

  def postmanVariableNameFrom(objReference: ObjectReference): PostmanVariableName = {
    import objReference._
    val name = s"$resourceType#$identifierField"
    PostmanVariableName(name)
  }

  def postmanVariableNameFrom(parameter: Parameter): PostmanVariableName = {
    val name = parameter.name
    PostmanVariableName(name)
  }

}
