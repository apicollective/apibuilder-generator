package models.generator.kotlin

import org.scalatest.{FunSpec, Matchers}
import KotlinTestHelper._

class KotlinGeneratorTest
  extends FunSpec
    with Matchers {

  import models.TestHelper._

  val serviceDefs = Seq(apidocApiService)

  describe("invoke should output Kotlin source files") {
    for (service <- serviceDefs) {
      it(s"for service [${service.name}]") {
        generateSourceFiles(service)
      }
    }
  }

  private val compileList = Seq(builtInTypesService, dateTimeService, generatorApiServiceWithUnionAndDescriminator)

  describe("Kotlin code compiles") {
    for (service <- compileList) {
      it(s"[${service.name}]") {
        val dir = generateSourceFiles(service)
        assertKotlinCodeCompiles(dir.toPath)
      }
    }
  }

}
