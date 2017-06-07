package scala.models.http4s

import scala.generator.ScalaDatatype
import scala.generator.ScalaPrimitive.{ObjectAsPlay, ObjectAsCirce}

import com.bryzek.apidoc.spec.v0.models.Service
import lib.Datatype

class ScalaService(service: Service) extends scala.generator.ScalaService(service) {
  override def scalaDatatype(t: Datatype): ScalaDatatype = convertObjectType(super.scalaDatatype(t))

  private def convertObjectType(sd: ScalaDatatype): ScalaDatatype = sd match {
    case ObjectAsPlay => ObjectAsCirce
    case ScalaDatatype.List(t) => ScalaDatatype.List(convertObjectType(t))
    case ScalaDatatype.Map(t) => ScalaDatatype.Map(convertObjectType(t))
    case ScalaDatatype.Option(t) => ScalaDatatype.Option(convertObjectType(t))
    case o => o
  }
}
