package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import io.apibuilder.spec.v0.models._
import scala.generator._

case class PlayResultPatternMatch(response: ScalaResponse, result: String) {
  def syntax: String = (response.code, response.datatype) match {
    case (ResponseCodeInt(code), ScalaPrimitive.Unit) => s"case ${result}.${ResultImpl(code, None, null).name} => new _root_.play.api.mvc.Results.Status(${code})"
    case (ResponseCodeInt(code), arg) => s"case ${result}.${ResultImpl(code, None, null).name}(_1) => new _root_.play.api.mvc.Results.Status(${code})(_root_.play.api.libs.json.Json.toJson(_1))"
    case _ => ""
  }
}

case class PlayResultDefConversion(operation: ScalaOperation, playResultPatternMatch: List[PlayResultPatternMatch]) {
  def arguments(responses: List[ScalaResponse]) = {
    @annotation.tailrec
    def contained(tpe: ScalaDatatype): ScalaDatatype = tpe match {
      case ScalaDatatype.List(i) => contained(i)
      case ScalaDatatype.Map(i) => contained(i)
      case ScalaDatatype.Option(i) => contained(i)
      case i => i
    }

    val writes = responses
      .map(_.datatype)
      .map(contained)
      .distinct
      .collect {
        case a: ScalaPrimitive.Model => a
        case a: ScalaPrimitive.Enum => a
        case a: ScalaPrimitive.Union => a
      }
      .map(arg => s"${arg.toVariableName}Writes: _root_.play.api.libs.json.Writes[${arg.name}]")
      .mkString(", ")

    writes match {
      case "" => ""
      case ws => s"(implicit ${ws})"
    }
  }

  def syntax: String = s"""
    def toPlayResult()${arguments(operation.responses.toList)}: _root_.play.api.mvc.Result = result match {
      ${playResultPatternMatch.map(_.syntax).mkString("\n")}
    }
  """
}

case class PlayResultImplicitConversion(operation: ScalaOperation, result: String, playResultDefConversion: PlayResultDefConversion) {
  def syntax: String = {
    val resultTrait = ResultTrait(operation)
    s"""
      implicit class ${resultTrait.name}Ops(val result: ${result}) extends AnyVal {
        ${playResultDefConversion.syntax}
      }
    """
  }
}

case class PlayResultImplicitConversions(name: String, playResultImplicitConversions: List[PlayResultImplicitConversion]) {
  def syntax: String = s"""
    object ${name} {
      ${playResultImplicitConversions.map(_.syntax).mkString("\n")}
    }
  """
}

case class PlayResultsPackage(packageObject: String, playResultImplicitConversions: List[PlayResultImplicitConversions]) {
  def syntax: String = s"""
    package ${packageObject.split('.').dropRight(1).mkString(".")}

    package object ${packageObject.split('.').last} {
      ${playResultImplicitConversions.map(_.syntax).mkString("\n")}
    }
  """
}

object ResultsForPlay extends Component {
  def playResultsPackage(service: ScalaService): PlayResultsPackage = {
    val resultsPackage = service.namespaces.results
    val playResultImplicitConversions = service.resources
      .map { resource =>
        val playResultImplicitConversions = resource.operations
          .map { operation =>
            val result = s"${resultsPackage}.${ResultsObject(resource, Nil).name}.${ResultTrait(operation).name}"

            val playPatternMatches = operation.responses.map(PlayResultPatternMatch(_, result)).toList
            val playResultDefConversion = PlayResultDefConversion(operation, playPatternMatches)
            PlayResultImplicitConversion(operation, result, playResultDefConversion)
          }
          .toList

        PlayResultImplicitConversions(resource.plural, playResultImplicitConversions)
      }
      .toList

    PlayResultsPackage(service.namespaces.resultsForPlay, playResultImplicitConversions)
  }

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val syntax = playResultsPackage(service).syntax
    Validated.validNel(syntax)
  }
}
