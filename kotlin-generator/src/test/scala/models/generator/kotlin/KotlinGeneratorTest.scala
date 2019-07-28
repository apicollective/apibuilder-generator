package models.generator.kotlin

import org.scalatest.{FunSpec, Matchers}
import KotlinTestHelper._

class KotlinGeneratorTest
  extends FunSpec
    with Matchers {

  import models.TestHelper._

  val serviceDefs = Seq(apidocApiService, generatorApiServiceWithUnionAndDescriminator, dateTimeService)

  describe("invoke should output Kotlin source files") {
    for (service <- serviceDefs) {
      it(s"for service [${service.name}]") {
        generateSourceFiles(service)
      }
    }
  }

  describe("built-in-types") {
    val service = builtInTypesService
    it(s"code compiles") {
      val dir = generateSourceFiles(service)
      assertKotlinCodeCompiles(dir.toPath)
    }
  }

}
