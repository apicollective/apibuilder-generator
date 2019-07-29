package models.generator.kotlin

import java.nio.file.Files
import com.fasterxml.jackson.databind.ObjectMapper
import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.Service
import models.TestHelper.{assertJodaTimeNotPresent, writeFiles}
import org.scalatest.Matchers

object KotlinTestHelper extends Matchers {

  def generateSourceFiles(service: Service): java.io.File = {
    val tmpDir = createTempDirectory(service)
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

  def assertKotlinCodeCompiles(kotlinSourceDirectory: java.nio.file.Path): Unit = {
    val msgCollector = KotlinCompiler.compile(kotlinSourceDirectory)
    msgCollector.hasErrors shouldBe false
  }

  private def createTempDirectory(service: Service): java.io.File = {
    val name = (service.name + System.currentTimeMillis).replace(' ', '-')
    val tmpdir = "/tmp"
    val dir = new java.io.File(tmpdir + "/" + name)
    dir.mkdirs
    dir.deleteOnExit
    dir
  }
}
