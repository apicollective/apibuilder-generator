package models.generator.kotlin

import org.scalatest.{FunSpec, Matchers}
import KotlinTestHelper._

class KotlinGeneratorTest
  extends FunSpec
    with Matchers {

  import models.TestHelper._

  private val serviceDefs = Seq(builtInTypesService,
                                dateTimeService,
                                generatorApiServiceWithUnionAndDescriminator,
                                apidocApiService,
                                collectionJsonDefaultsService,
                                referenceApiService,
                                referenceWithImportsApiService)

  describe("Kotlin code compiles") {
    for (service <- serviceDefs) {
      it(s"[${service.name}] imports=${(service.imports.size > 0)}") {
        val dir = generateSourceFiles(service)
        assertKotlinCodeCompiles(dir)
      }
    }
  }

}
