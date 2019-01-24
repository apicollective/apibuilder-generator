package scala.models.play.files

import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.models.play.gens._
import scala.models.play.Helpers.compareWithoutWhiteSpaces
import scala.generator.{ScalaEnum, ScalaModel, ScalaUnion}

class ModelsGensSpec extends FunSpec with Matchers with PropertyChecks {

  implicit val scalacheckConfig = generatorDrivenConfig.copy(sizeRange = 10)

  it("generates joda date time generator") {
    forAll { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""
        private[${ns.last}] ${ModelsGens.arbitrary(ns, "JodaDateTime", ModelsGens.JodaDateTime)}
        private[${ns.last}] lazy val genJodaDateTime: ${ModelsGens.Gen}[${ModelsGens.JodaDateTime}] = ${ModelsGens.Gen}.lzy {
          ${ModelsGens.Gen}.posNum[Long].map(instant => new ${ModelsGens.JodaDateTime}(instant))
        }
      """

      val result = ModelsGens.jodaDateTimeGenAndArbitrary(ns)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates joda local date generator") {
    forAll { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""
        private[${ns.last}] ${ModelsGens.arbitrary(ns, "JodaLocalDate", ModelsGens.JodaLocalDate)}
        private[${ns.last}] lazy val genJodaLocalDate: ${ModelsGens.Gen}[${ModelsGens.JodaLocalDate}] = ${ModelsGens.Gen}.lzy {
          ${ModelsGens.Gen}.posNum[Long].map(instant => new ${ModelsGens.JodaLocalDate}(instant))
        }
      """

      val result = ModelsGens.jodaLocalDateGenAndArbitrary(ns)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates play js object generator") {
    forAll { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""
        private[${ns.last}] ${ModelsGens.arbitrary(ns, "JsObject", ModelsGens.JsObject)}
        private[${ns.last}] lazy val genJsObject: ${ModelsGens.Gen}[${ModelsGens.JsObject}] = ${ModelsGens.Gen}.lzy {
          for {
            underlying <- ${ModelsGens.Arbitrary}.arbitrary[Map[String, ${ModelsGens.JsValue}]]
          } yield ${ModelsGens.JsObject}(underlying)
        }
      """

      val result = ModelsGens.playJsObjectGenAndArbitrary(ns)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates play js value generator") {
    forAll { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""
        private[${ns.last}] ${ModelsGens.arbitrary(ns, "JsValue", ModelsGens.JsValue)}
        private[${ns.last}] lazy val genJsValue: ${ModelsGens.Gen}[${ModelsGens.JsValue}] = ${ModelsGens.Gen}.lzy {
          ${ModelsGens.Gen}.oneOf(
            ${ModelsGens.Arbitrary}.arbitrary[IndexedSeq[${ModelsGens.JsValue}]].map(${ModelsGens.JsArray}),
            ${ModelsGens.Arbitrary}.arbitrary[Boolean].map(${ModelsGens.JsBoolean}),
            ${ModelsGens.Gen}.const(${ModelsGens.JsNull}),
            ${ModelsGens.Arbitrary}.arbitrary[BigDecimal].map(${ModelsGens.JsNumber}),
            // ${ModelsGens.Arbitrary}.arbitrary[${ModelsGens.JsObject}],
            ${ModelsGens.Arbitrary}.arbitrary[String].map(${ModelsGens.JsString})
          )
        }
      """

      val result = ModelsGens.playJsValueGenAndArbitrary(ns)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates imports for model") (pending)
  it("generates imports for namespace, and type") (pending)

  it("generates arbitrary given a namespace, name, and type") {
    forAll { (str: String, name: String, tpe: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""implicit lazy val arbitrary${ns.models.split('.').map(_.capitalize).mkString}${name}: ${ModelsGens.Arbitrary}[$tpe] = ${ModelsGens.Arbitrary}(gen${name})"""

      val result = ModelsGens.arbitrary(ns, name, tpe)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates arbitrary for enum based on namespace, name, and qualifiedName") {
    forAll { enum: ScalaEnum =>
      val expected = ModelsGens.arbitrary(enum.ssd.namespaces, enum.name, enum.qualifiedName)
      val result = ModelsGens.arbitrary(enum)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates arbitrary for model based on namespace, name, and qualifiedName") {
    forAll { model: ScalaModel =>
      val expected = ModelsGens.arbitrary(model.ssd.namespaces, model.name, model.qualifiedName)
      val result = ModelsGens.arbitrary(model)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates arbitrary for union based on namespace, name, and qualifiedName") {
    forAll { union: ScalaUnion =>
      val expected = ModelsGens.arbitrary(union.ssd.namespaces, union.name, union.qualifiedName)
      val result = ModelsGens.arbitrary(union)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates gen for list of possible generators") {
    forAll { (name: String, tpe: String, list: List[String]) =>
      val expected = list match {
        case Nil => ""
        case one :: Nil => s"lazy val gen${name}: ${ModelsGens.Gen}[${tpe}] = ${one}"
        case list => s"""
          lazy val gen${name}: ${ModelsGens.Gen}[${tpe}] = ${ModelsGens.Gen}.lzy {
            ${ModelsGens.Gen}.oneOf(${list.mkString(", ")})
          }
        """
      }
      val result = ModelsGens.genOneOf(name, tpe, list)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates gen for element with list of attributes") {
    forAll { (name: String, tpe: String, properties: List[(String, String)]) =>
      val expected = properties match {
        case Nil => ""
        case arguments => s"""
        lazy val gen${name}: ${ModelsGens.Gen}[${tpe}] = ${ModelsGens.Gen}.lzy {
          for {
            ${properties.map { case (name, tpe) => s"${name} <- ${ModelsGens.Arbitrary}.arbitrary[${tpe}]" }.mkString("\n")}
          } yield ${tpe}(${properties.unzip._1.mkString(",")})
        }
        """
      }
      val result = ModelsGens.genFor(name, tpe, properties)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates gen for enum") (pending)
  it("generates gen for model") (pending)
  it("generates gen for union") (pending)

}
