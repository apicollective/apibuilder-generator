package models.generator.kotlin

import com.fasterxml.jackson.databind.ObjectMapper
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FlatSpec, Matchers}

class KotlinGeneratorTest
  extends FlatSpec
    with Matchers {

  "generateJacksonObjectMapper" should "output source file" in {
    val generator = new KotlinGenerator()
    val file = generator.generateJacksonObjectMapper()
    file.name shouldBe "JacksonObjectMapperFactory.kt"
    file.contents should include (classOf[ObjectMapper].getSimpleName)
  }

  "invoke" should "output source files" in {
    val service = models.TestHelper.generatorApiService
    val invocationForm = new InvocationForm(service, Seq.empty, None)
    val generator = new KotlinGenerator()
    val files = generator.invoke(invocationForm).right.get
    files.size shouldBe > (0)
    files.foreach(f => {
      f.contents.length shouldBe > (0)
      f.name should endWith (".kt")
    })
  }
}
