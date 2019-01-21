package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import io.apibuilder.spec.v0.models._
import scala.generator._

case class ResultTrait(operation: ScalaOperation) {
  def name = s"${operation.name.capitalize}Result"
  def syntax: String = s"""sealed trait ${name}"""
}

case class ResultImpl(code: Int, arg: Option[ScalaDatatype], resultTrait: ResultTrait) {
  def name: String = s"HTTP${code}"
  def syntax: String = {
    arg match {
      case Some(arg) => s"case class ${name}(${ScalaUtil.quoteNameIfKeyword(arg.toVariableName)}: ${arg.name}) extends ${resultTrait.name}"
      case None =>         s"case object ${name} extends ${resultTrait.name}"
    }
  }
}

case class ResultObject(resultImpls: List[ResultImpl]) {
  def syntax: String = resultImpls.headOption.fold("") { resultImpl =>
    s"""
      ${resultImpl.resultTrait.syntax}
      object ${resultImpl.resultTrait.name} {
        ${resultImpls.map(_.syntax).mkString("\n")}
      }
    """
  }
}

case class ResultsObject(resource: ScalaResource, resultObjects: List[ResultObject]) {
  def name = resource.plural
  def syntax: String = s"""
    object ${name} {
      ${resultObjects.map(_.syntax).mkString("\n")}
    }
  """
}

case class ResultsPackage(packageObject: String, resultsObjects: List[ResultsObject]) {
  def syntax: String = s"""
    package ${packageObject.split('.').dropRight(1).mkString(".")}

    package object ${packageObject.split('.').last} {
      ${resultsObjects.map(_.syntax).mkString("\n")}
    }
  """
}

object Results extends Component {
  def resultImpls(operation: ScalaOperation): List[ResultImpl] = {
    val resultTrait = ResultTrait(operation)
    operation.responses
      .map(r => (r, r.code))
      .collect { case (r, ResponseCodeInt(code)) => (code, r.datatype) }
      .map {
        case (code, ScalaPrimitive.Unit) =>  ResultImpl(code, None, resultTrait)
        case (code, tpe) =>  ResultImpl(code, tpe.some, resultTrait)
      }
      .toList
  }

  def resultsObject(resource: ScalaResource): ResultsObject = {
    val resultObjects = resource.operations
      .map(resultImpls)
      .map(ResultObject)
      .toList

    ResultsObject(resource, resultObjects)
  }

  def resultsPackage(service: ScalaService): ResultsPackage = {
    val resultsObjects = service.resources.map(resultsObject).toList
    ResultsPackage(service.namespaces.results, resultsObjects)
  }

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val syntax = resultsPackage(service).syntax
    Validated.validNel(syntax)
  }
}
