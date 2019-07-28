package models.generator.kotlin

import com.fasterxml.jackson.databind.ObjectMapper
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import org.scalatest.{FunSpec, Matchers}
import java.nio.file.Files.createTempDirectory

import KotlinTestHelper._
import io.apibuilder.spec.v0.models.Service

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

  private def generateSourceFiles(service: Service): java.io.File = {
    val tmpDir = createTempDirectory(getClass().getSimpleName).toFile
    tmpDir.deleteOnExit()
    val invocationForm = InvocationForm(service, Seq.empty, None)
    val generator = new KotlinGenerator()
    val files = generator.invoke(invocationForm).right.get
    files.size shouldBe >(0)
    files.foreach(f => {
      f.contents.length shouldBe >(0)
      f.name should endWith(".kt")
    })
    files.exists(
      file => (file.name == "JacksonObjectMapperFactory.kt" && file.contents.contains(classOf[ObjectMapper].getSimpleName))
    ) shouldBe true
    assertJodaTimeNotPresent(files)
    writeFiles(tmpDir, files)
    tmpDir
  }

  private def enumFileExists(files: Seq[File], enumName: String): Boolean = {
    files.exists(file => {
      file.contents.contains(s"enum class ${enumName}")
    })
  }
}
