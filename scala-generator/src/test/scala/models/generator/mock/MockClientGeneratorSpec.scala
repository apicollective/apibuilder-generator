package scala.generator.mock

import org.scalatest.{FunSpec, Matchers}

import scala.generator.mock.MockClientGenerator._
import scala.generator.ScalaField.Limitation

class MockClientGeneratorSpec extends FunSpec with Matchers {
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
    calculateStringLength(Limitation(None, Some(-1))) should be(0)

    calculateStringLength(Limitation(Some(Int.MaxValue), None)) should be(Int.MaxValue) // TODO: really?!
    calculateStringLength(Limitation(Some(Long.MaxValue), None)) should be(Int.MaxValue) // TODO: really?!
  }
}
