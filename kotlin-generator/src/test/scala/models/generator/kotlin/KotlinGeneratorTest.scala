package models.generator.kotlin

import io.apibuilder.generator.v0.models.InvocationForm
import models.generator.kotlin.KotlinTestHelper._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class KotlinGeneratorTest
  extends AnyFunSpec
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
      val files = generator.invoke(invocationForm).getOrElse(sys.error("got Left"))
      assertPackageExists("io.apibuilder.reference.api.v0.models", files)
      assertPackageExists("io.apibuilder.reference.api.v0.enums", files)
    }
  }

  describe("Union") {
    val service = generatorApiServiceWithUnionAndDiscriminator
    it(s"[${service.name}] completeness check") {
      service.models.map(_.name) shouldBe Seq("guest_user", "registered_user")
      service.unions.map(_.name) shouldBe Seq("user")
      val unionTypes = service.unions.flatMap(_.types)
      unionTypes.map(_.`type`) shouldBe Seq("registered_user", "guest_user", "system_user", "string")
      val invocationForm = InvocationForm(service, Seq.empty, None)
      val generator = new KotlinGenerator()
      val files = generator.invoke(invocationForm).getOrElse(sys.error("got Left"))

      // model: registered_user
      val registeredUser = getFile("RegisteredUser.kt", files)
      registeredUser.dir.get should endWith ("/models")
      assertFileContainsString("data class RegisteredUser(", registeredUser)
      assertFileContainsString("@JsonProperty(\"email\")", registeredUser)
      assertFileContainsString("@JsonProperty(\"id\")", registeredUser)

      // model: guest_user
      val guestUser = getFile("GuestUser.kt", files)
      guestUser.dir.get should endWith ("/models")
      assertFileContainsString("data class GuestUser(", guestUser)
      assertFileContainsString("@JsonProperty(\"email\")", guestUser)
      assertFileContainsString("@JsonProperty(\"id\")", guestUser)

      // union: user
      val user = getFile("User.kt", files)
      user.dir.get should endWith ("/models")
      assertFileContainsString("@JsonTypeInfo(", user)
      assertFileContainsString("@JsonSubTypes(", user)
      assertFileContainsString("JsonSubTypes.Type(", user)
      assertFileContainsString("@JsonProperty(\"email\")", user)
      assertFileContainsString("@JsonProperty(\"id\")", user)
      assertFileContainsString("data class GuestUser(", user)
      assertFileContainsString("data class RegisteredUser(", user)
      assertFileContainsString("object UserUndefined", user)
    }
  }
}
