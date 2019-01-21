package scala.models.play.components

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import io.apibuilder.spec.v0.models._
import scala.generator._

case class MethodDef(operation: ScalaOperation, result: String) {
  def syntax: String = s"""def ${operation.name}(${operation.argList().getOrElse("")}): F[${result}]"""
}

case class ControllerTrait(name: String, methods: List[MethodDef]) {
  def syntax: String = s"""
    trait ${name}[F[_]] {
      ${methods.map(_.syntax).mkString("\n")}
    }
  """
}

case class ControllersPackage(packageObject: String, controllerTraits: List[ControllerTrait]) {
  def syntax: String = s"""
    package ${packageObject.split('.').dropRight(1).mkString(".")}

    package object ${packageObject.split('.').last} {
      ${controllerTraits.map(_.syntax).mkString("\n")}
    }
  """
}

object Controllers extends Component {
  def controllerTrait(resource: ScalaResource, resultsPackage: String): ControllerTrait = {
    val name = resource.plural
    val methods = resource.operations
      .map { operation =>
        val result = s"${resultsPackage}.${ResultsObject(resource, Nil).name}.${ResultTrait(operation).name}"
        MethodDef(operation, result)
      }
      .toList

    ControllerTrait(name, methods)
  }

  def controllersPackage(service: ScalaService, resultsPackage: String): ControllersPackage = {
    val controllerTraits = service.resources
      .map(controllerTrait(_, resultsPackage))
      .toList

    ControllersPackage(service.namespaces.controllers, controllerTraits)
  }

  def code(service: ScalaService): ValidatedNel[String, String] = {
    val syntax = controllersPackage(service, service.namespaces.results).syntax
    Validated.validNel(syntax)
  }
}
