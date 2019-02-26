package models.attributes

import io.flow.postman.generator.attributes.v0.models.ObjectReference

object PostmanAttributes {

  def postmanVariableNameFrom(objReference: ObjectReference): String = {
    import objReference._
    s"$resourceType#$identifierField"
  }

  def postmanVariableRefFrom(objectReference: ObjectReference): String = {
    val variableName = postmanVariableNameFrom(objectReference)
    s"{{$variableName}}"
  }

}
