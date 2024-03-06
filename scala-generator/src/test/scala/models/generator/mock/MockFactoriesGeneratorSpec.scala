package scala.generator.mock

import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.{ScalaClientMethodConfigs, ScalaService}
import scala.generator.ScalaField.Limitation
import scala.models.{Attributes, DateTimeTypeConfig, DateTypeConfig}

class MockFactoriesGeneratorSpec extends AnyFunSpec with Matchers {
  import scala.generator.mock.MockFactoriesGenerator._

  it("should generate the right desired length for string given a field limitation") {
    calculateStringLength(Limitation(None, None)) should be(24)
    calculateStringLength(Limitation(Some(6), None)) should be(24) // TODO or [6, 24] ??? -> mostly for very small min vals e.g. min=3
    calculateStringLength(Limitation(Some(25), None)) should be(25)
    calculateStringLength(Limitation(None, Some(23))) should be(23)
    // TODO or [24, 10000] ??? hmm, rather not ==> leave 24
    //   this is not symmetric problem -- as min has a natural lower bound of 0, while max has a very high upper bound
    calculateStringLength(Limitation(None, Some(10000))) should be(24)

    val lenFrom22And25 = calculateStringLength(Limitation(Some(22), Some(25)))
    lenFrom22And25 should be >= 22
    lenFrom22And25 should be <= 25

    val lenFrom6And8 = calculateStringLength(Limitation(Some(6), Some(8)))
    lenFrom6And8 should be >= 6
    lenFrom6And8 should be <= 8

    calculateStringLength(Limitation(Some(3), Some(3))) should be(3)

    calculateStringLength(Limitation(Some(25), Some(22))) should be(0)
    calculateStringLength(Limitation(Some(22), Some(25))) should be(23)
    calculateStringLength(Limitation(None, Some(-1))) should be(0)

    calculateStringLength(Limitation(Some(Int.MaxValue), None)) should be(Int.MaxValue) // TODO: really?!
    calculateStringLength(Limitation(Some(Long.MaxValue), None)) should be(Int.MaxValue) // TODO: really?!
  }

  it("creates factory methods for the models ") {
    val service = models.TestHelper.referenceApiService
    val ssd = new ScalaService(service, Attributes.Http4sDefaultConfig)

    val sourceCode = new MockFactoriesGenerator(ssd, None).generateCode()

    assertValidScalaSourceCode(sourceCode)
    models.TestHelper.assertEqualsFile(s"/scala-mock-factories-reference-service.txt", sourceCode)
  }
}
