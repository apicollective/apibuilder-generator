package models.generator.csv

import io.apibuilder.generator.v0.models.File
import io.apibuilder.spec.v0.models._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CsvGeneratorTest extends AnyFunSpec with Matchers {

  val NEWLINE = "\r\n"
  val QUOTE = "\""

  val generator = new CsvGenerator

  describe("generateSourceFile") {
    it("generates the file") {
      assert(generator.generateSourceFile("output.csv", Seq("field one", "field two"), Seq(Seq("one", "two"), Seq("three", "four"))) === File(
        name = "output.csv",
        contents = s"field one,field two${NEWLINE}one,two${NEWLINE}three,four${NEWLINE}")
      )
    }
    it("handles commas") {
      assert(generator.generateSourceFile("output.csv", Seq("field one", "field two"), Seq(Seq("one, one", "two"), Seq("three", "four"))) === File(
        name = "output.csv",
        contents = s"field one,field two${NEWLINE}${QUOTE}one, one${QUOTE},two${NEWLINE}three,four${NEWLINE}")
      )
    }
    it("handles quotes") {
      assert(generator.generateSourceFile("output.csv", Seq("field one", "field two"), Seq(Seq("one \"one\"", "two"), Seq("three", "four"))) === File(
        name = "output.csv",
        contents = s"field one,field two${NEWLINE}${QUOTE}one ${QUOTE}${QUOTE}one${QUOTE}${QUOTE}${QUOTE},two${NEWLINE}three,four${NEWLINE}")
      )
    }
    it("handles comma and quotes") {
      assert(generator.generateSourceFile("output.csv", Seq("field one", "field two"), Seq(Seq("one, \"one\"", "two"), Seq("three", "four"))) === File(
        name = "output.csv",
        contents = s"field one,field two${NEWLINE}${QUOTE}one, ${QUOTE}${QUOTE}one${QUOTE}${QUOTE}${QUOTE},two${NEWLINE}three,four${NEWLINE}")
      )
    }
  }

  val operation = Operation(
    method = Method.Get,
    path = "/wheels",
    description = Some("get wheels")
  )

  val operationWithComma = operation.copy(description = Some("get, wheels"))

  val resource = Resource(
    `type` = "car",
    plural = "cars",
    path = Some("/cars"),
    description = Some("cars description"),
    operations = Seq(operation)
  )

  describe("generateResourcesFile") {
    it("generates the file") {
      assert(generator.generateResourcesFile(Seq(resource)) === File(
        name = "resources.csv",
        contents = s"method,path,description${NEWLINE}GET,/cars/wheels,get wheels${NEWLINE}")
      )
    }
  }

}
