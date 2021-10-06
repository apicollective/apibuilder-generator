package scala.models.play.files

import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.generator.{ScalaEnum, ScalaModel, ScalaService, ScalaUnion}
import scala.models.Attributes
import scala.models.play.Helpers
import scala.models.play.Helpers.compareWithoutWhiteSpaces
import scala.models.play.gens._

class ModelsGensSpec extends AnyFunSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  private[this] implicit val scalacheckConfig: PropertyCheckConfiguration = generatorDrivenConfig.copy(sizeRange = 10)

  it("generates joda DateTime generator") {
    forAll(genNsStr) { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val scalaService = ScalaService(service = Helpers.basicService(str))
      val expected = s"""
        private[${ns.last}] implicit lazy val arbitraryDateTime: _root_.org.scalacheck.Arbitrary[_root_.org.joda.time.DateTime] = _root_.org.scalacheck.Arbitrary(genDateTime)
        private[${ns.last}] lazy val genDateTime: _root_.org.scalacheck.Gen[_root_.org.joda.time.DateTime] = _root_.org.scalacheck.Gen.lzy {
          _root_.org.scalacheck.Gen.posNum[Long].map(instant => new _root_.org.joda.time.DateTime(instant))
        }
      """

      val result = ModelsGens.dateTimeGenAndArbitrary(scalaService)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates joda LocalDate generator") {
    forAll(genNsStr) { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val scalaService = ScalaService(service = Helpers.basicService(str))
      val expected = s"""
        private[${ns.last}] implicit lazy val arbitraryLocalDate: _root_.org.scalacheck.Arbitrary[_root_.org.joda.time.LocalDate] = _root_.org.scalacheck.Arbitrary(genLocalDate)
        private[${ns.last}] lazy val genLocalDate: _root_.org.scalacheck.Gen[_root_.org.joda.time.LocalDate] = _root_.org.scalacheck.Gen.lzy {
          _root_.org.scalacheck.Gen.posNum[Long].map(instant => new _root_.org.joda.time.LocalDate(instant))
        }
      """

      val result = ModelsGens.dateGenAndArbitrary(scalaService)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates java OffsetDateTime generator") {
    forAll(genNsStr) { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val scalaService = ScalaService(service = Helpers.basicService(str), Attributes.PlayGen2DefaultConfig)
      val expected = s"""
        private[${ns.last}] implicit lazy val arbitraryOffsetDateTime: _root_.org.scalacheck.Arbitrary[_root_.java.time.OffsetDateTime] = _root_.org.scalacheck.Arbitrary(genOffsetDateTime)
        private[${ns.last}] lazy val genOffsetDateTime: _root_.org.scalacheck.Gen[_root_.java.time.OffsetDateTime] = _root_.org.scalacheck.Gen.lzy {
          _root_.org.scalacheck.Gen.posNum[Long].map(instant => new _root_.java.time.OffsetDateTime(instant))
        }
      """

      val result = ModelsGens.dateTimeGenAndArbitrary(scalaService)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates java LocalDate generator") {
    forAll(genNsStr) { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val scalaService = ScalaService(service = Helpers.basicService(str), Attributes.PlayGen2DefaultConfig)
      val expected = s"""
        private[${ns.last}] implicit lazy val arbitraryLocalDate: _root_.org.scalacheck.Arbitrary[_root_.java.time.LocalDate] = _root_.org.scalacheck.Arbitrary(genLocalDate)
        private[${ns.last}] lazy val genLocalDate: _root_.org.scalacheck.Gen[_root_.java.time.LocalDate] = _root_.org.scalacheck.Gen.lzy {
          _root_.org.scalacheck.Gen.posNum[Long].map(instant => new _root_.java.time.LocalDate(instant))
        }
      """

      val result = ModelsGens.dateGenAndArbitrary(scalaService)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates play js object generator") {
    forAll(genNsStr) { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""
        private[${ns.last}] ${ModelsGens.arbitrary(ns, "JsObject", ModelsGens.JsObject)}
        private[${ns.last}] lazy val genJsObject: _root_.org.scalacheck.Gen[${ModelsGens.JsObject}] = _root_.org.scalacheck.Gen.lzy {
          for {
            underlying <- _root_.org.scalacheck.Arbitrary.arbitrary[Map[String, ${ModelsGens.JsValue}]]
          } yield ${ModelsGens.JsObject}(underlying)
        }
      """

      val result = ModelsGens.playJsObjectGenAndArbitrary(ns)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates play js value generator") {
    forAll(genNsStr) { (str: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""
        private[${ns.last}] ${ModelsGens.arbitrary(ns, "JsValue", ModelsGens.JsValue)}
        private[${ns.last}] lazy val genJsValue: _root_.org.scalacheck.Gen[${ModelsGens.JsValue}] = _root_.org.scalacheck.Gen.lzy {
          _root_.org.scalacheck.Gen.oneOf(
            _root_.org.scalacheck.Arbitrary.arbitrary[IndexedSeq[${ModelsGens.JsValue}]].map(${ModelsGens.JsArray}),
            _root_.org.scalacheck.Arbitrary.arbitrary[Boolean].map(${ModelsGens.JsBoolean}),
            _root_.org.scalacheck.Gen.const(${ModelsGens.JsNull}),
            _root_.org.scalacheck.Arbitrary.arbitrary[BigDecimal].map(${ModelsGens.JsNumber}),
            // _root_.org.scalacheck.Arbitrary.arbitrary[${ModelsGens.JsObject}],
            _root_.org.scalacheck.Arbitrary.arbitrary[String].map(${ModelsGens.JsString})
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
    forAll(genNsStr, Gen.alphaNumStr, Gen.alphaNumStr) { (str: String, name: String, tpe: String) =>
      val ns = scala.generator.Namespaces(str)
      val expected = s"""implicit lazy val arbitrary${name}: _root_.org.scalacheck.Arbitrary[$tpe] = _root_.org.scalacheck.Arbitrary(gen${name})"""

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
        case one :: Nil => s"lazy val gen${name}: _root_.org.scalacheck.Gen[${tpe}] = ${one}"
        case list => s"""
          lazy val gen${name}: _root_.org.scalacheck.Gen[${tpe}] = _root_.org.scalacheck.Gen.lzy {
            _root_.org.scalacheck.Gen.oneOf(${list.mkString(", ")})
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
        case _ => s"""
        lazy val gen${name}: _root_.org.scalacheck.Gen[${tpe}] = _root_.org.scalacheck.Gen.lzy {
          for {
            ${properties.map { case (name, tpe) => s"${name} <- _root_.org.scalacheck.Arbitrary.arbitrary[${tpe}]" }.mkString("\n")}
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
