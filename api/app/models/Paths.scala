package models

import lib.Text
import com.gilt.apidocgenerator.models.{Operation, Resource}

object Paths {

  def resource(modelName: String, modelPlural: Option[String] = None, resource: Resource): String = {
    resource.path.getOrElse {
      "/" + modelPlural.getOrElse(Text.pluralize(modelName))
    }
  }

  def operation(modelName: String, modelPlural: Option[String] = None, resource: Resource, operation: Operation): String = {
    operation.path.getOrElse {
      Paths.resource(modelName, modelPlural, resource)
    }
  }

}
