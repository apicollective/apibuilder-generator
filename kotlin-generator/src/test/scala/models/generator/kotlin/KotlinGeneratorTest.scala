package models.generator.kotlin

import org.scalatest.{FunSpec, Matchers}
import KotlinTestHelper._
import io.apibuilder.generator.v0.models.InvocationForm

class KotlinGeneratorTest
  extends FunSpec
    with Matchers {

  import models.TestHelper._

  private val serviceDefs = Seq(builtInTypesService,
                                // dateTimeService,
                                // generatorApiServiceWithUnionAndDescriminator,
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

  describe("Package names") {
    val service = referenceApiService
    it(s"[${service.name}]") {
      service.enums.size shouldBe > (0)
      service.namespace shouldBe "io.apibuilder.reference.api.v0"
      val invocationForm = InvocationForm(service, Seq.empty, None)
      val generator = new KotlinGenerator()
      val files = generator.invoke(invocationForm).right.get
      assertPackageExists("io.apibuilder.reference.api.v0.models", files)
      assertPackageExists("io.apibuilder.reference.api.v0.enums", files)
    }
  }

  describe("Union") {
    val service = generatorApiServiceWithUnionAndDescriminator
    it(s"[${service.name}] sanity check") {
      service.unions.size shouldBe > (0)
      val invocationForm = InvocationForm(service, Seq.empty, None)
      val generator = new KotlinGenerator()
      val files = generator.invoke(invocationForm).right.get

      val registeredUser = getFile("RegisteredUser.kt", files)
      assertFileContainsString("data class RegisteredUser(", registeredUser)
      assertFileContainsString("@JsonProperty(\"email\")", registeredUser)
      assertFileContainsString("@JsonProperty(\"id\")", registeredUser)

      val guestUser = getFile("GuestUser.kt", files)
      assertFileContainsString("data class GuestUser(", guestUser)
      assertFileContainsString("@JsonProperty(\"email\")", guestUser)
      assertFileContainsString("@JsonProperty(\"id\")", guestUser)
    }
  }
}
