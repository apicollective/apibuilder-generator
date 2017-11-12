package models.generator.kotlin

import com.fasterxml.jackson.databind.ObjectMapper
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FlatSpec, Matchers}
import java.nio.file.Files.createTempDirectory
import KotlinTestHelper._

class KotlinGeneratorTest
  extends FlatSpec
    with Matchers {

  "invoke" should "output Kotlin source files" in {
    val tmpDir = createTempDirectory(getClass().getSimpleName).toFile
    tmpDir.deleteOnExit()
    val service = models.TestHelper.generatorApiService
    val invocationForm = new InvocationForm(service, Seq.empty, None)
    val generator = new KotlinGenerator()
    val files = generator.invoke(invocationForm).right.get
    writeFiles(tmpDir, files)
    files.size shouldBe > (0)
    files.foreach(f => {
      f.contents.length shouldBe > (0)
      f.name should endWith (".kt")
      // assertValidKotlinSourceCode(tmpDir)
     })
    files.exists(
      file => (file.name == "JacksonObjectMapperFactory.kt" && file.contents.contains(classOf[ObjectMapper].getSimpleName))
    ) shouldBe true

  }
}
