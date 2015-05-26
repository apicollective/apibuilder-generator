package scala.generator

import scala.annotation.tailrec

import ScalaPrimitive._
import ScalaDatatype._

import org.scalatest.{FunSpec, Matchers}

class ScalaDatatypeSpec extends FunSpec with Matchers {

  it("should generate the right variable name when nested") {
    val model = new Model("org.example", "User")
    val string = ScalaPrimitive.String

    @tailrec
    def nest[T <: ScalaDatatype](
      d: ScalaDatatype, nester: ScalaDatatype => T, levels: Int
    ): ScalaDatatype = {
      if (levels <= 0) d else nest(nester(d), nester, levels - 1)
    }

    def nestList(d: ScalaDatatype, levels: Int): ScalaDatatype = {
      nest(d, List(_), levels)
    }

    def nestMap(d: ScalaDatatype, levels: Int): ScalaDatatype = {
      nest(d, Map(_), levels)
    }

    nestList(model, 0).toVariableName should be("user")
    nestList(model, 1).toVariableName should be("users")
    nestList(model, 2).toVariableName should be("users")
    nestList(model, 3).toVariableName should be("users")

    nestMap(model, 0).toVariableName should be("user")
    nestMap(model, 1).toVariableName should be("users")
    nestMap(model, 2).toVariableName should be("users")
    nestMap(model, 3).toVariableName should be("users")

    nestList(string, 0).toVariableName should be("value")
    nestList(string, 1).toVariableName should be("values")
    nestList(string, 2).toVariableName should be("values")
    nestList(string, 3).toVariableName should be("values")

    nestMap(string, 0).toVariableName should be("value")
    nestMap(string, 1).toVariableName should be("values")
    nestMap(string, 2).toVariableName should be("values")
    nestMap(string, 3).toVariableName should be("values")
  }

}


