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
      findPathParams(operation.path)
        .map(param => (param, resource))
    }
    pathsWithOperation
      .groupBy{case (param, _) => param}
      .mapValues(_.size)
      .filter(_._2 >= IdFieldHeuristicThreshold)
      .map(_._1)
      .headOption
  }

  def findPathParams(path: String): Seq[String] = {
    val regex = """\:(\w+)[\/]{0,1}""".r
    regex
      .findAllIn(path)
      .map(str => regex.replaceAllIn(str, "$1"))
      .toList
  }
}
