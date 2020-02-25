package generator

import io.apibuilder.spec.v0.models.Resource

object Heuristics {

  // TODO: unify heuristics mechanism with PostmanAttributes.PostmanVariableName type and its var name->reference logic
  case class PathVariable(name: String, postmanVarName: String) {
    def postmanVarRef: String = s"{{$postmanVarName}}"
  }

  val IdFieldHeuristicThreshold = 3

  def idFieldHeuristic(resource: Resource): Option[String] = {
    val pathsWithOperation = resource.operations.flatMap { operation =>
      PathParamsFinder.find(operation.path)
        .map(param => (param, resource))
    }
    pathsWithOperation
      .groupBy { case (param, _) => param }
      .view
      .mapValues(_.size)
      .filter(_._2 >= IdFieldHeuristicThreshold)
      .keys
      .headOption
  }

}
