package models.generator.kotlin

import com.fasterxml.jackson.databind.ObjectMapper
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import org.scalatest.{FunSpec, Matchers}
import java.nio.file.Files.createTempDirectory

import KotlinTestHelper._

class KotlinGeneratorTest
  extends FunSpec
    with Matchers {

  import models.TestHelper._

  val serviceDefs = Seq(apidocApiService, generatorApiServiceWithUnionAndDescriminator)

  describe("invoke should output Kotlin source files") {
    for (service <- serviceDefs) {
      it(s"for service [${service.name}]") {
        val tmpDir = createTempDirectory(getClass().getSimpleName).toFile
        tmpDir.deleteOnExit()
        val service = models.TestHelper.apidocApiService
        service.enums.size shouldBe (3)
        val invocationForm = new InvocationForm(service, Seq.empty, None)
        val generator = new KotlinGenerator()
        val files = generator.invoke(invocationForm).right.get
        assertJodaTimeNotPresent(files)
        writeFiles(tmpDir, files)
        files.size shouldBe >(0)
        files.foreach(f => {
          f.contents.length shouldBe >(0)
          f.name should endWith(".kt")
        })
        // TODO assertValidKotlinSourceCode(tmpDir.toPath)
        files.exists(
          file => (file.name == "JacksonObjectMapperFactory.kt" && file.contents.contains(classOf[ObjectMapper].getSimpleName))
        ) shouldBe true

        Seq("Visibility", "Publication", "OriginalType").foreach { enumName =>
          enumFileExists(files, enumName) shouldBe true
        }
      }
    }
  }

  private def enumFileExists(files: Seq[File], enumName: String): Boolean = {
    files.exists(file => {
      file.contents.contains(s"enum class ${enumName}")
    })
  }
}
