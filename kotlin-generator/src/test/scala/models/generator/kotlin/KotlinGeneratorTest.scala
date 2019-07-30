package models.generator.kotlin

import org.scalatest.{FunSpec, Matchers}
import KotlinTestHelper._

class KotlinGeneratorTest
  extends FunSpec
    with Matchers {

  import models.TestHelper._

  private val compileList = Seq(builtInTypesService,
                              dateTimeService,
                              generatorApiServiceWithUnionAndDescriminator,
                              apidocApiService)

  describe("Kotlin code compiles") {
    for (service <- compileList) {
      it(s"[${service.name}]") {
        val dir = generateSourceFiles(service)
        assertKotlinCodeCompiles(dir.toPath)
      }
    }
  }

}
