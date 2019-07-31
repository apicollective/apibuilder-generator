package models.generator.kotlin

import java.nio.file.Files

import models.TestHelper
import org.scalatest.{FunSpec, Matchers}

class KotlinCompilerSpec
  extends FunSpec
  with Matchers {

  private val goodCode = "class HelloWorld(val name: String?) { /* hello */ }"

  describe("KotlinCompiler") {
    it("happy path") {
      assert(compileCode(goodCode))
    }

    it("bad code") {
      compileCode("{") shouldBe (false)
      compileCode("}") shouldBe (false)
      compileCode("import kotlin.collections.BogusList") shouldBe (false)
    }
  }

  private def compileCode(sourceCode: String): Boolean = {
    val tmpDir = Files.createTempDirectory(this.getClass.getName).toFile
    val sourceFile = new java.io.File(s"${tmpDir.toString}/HelloWorld.kt")
    TestHelper.writeToFile(sourceFile.toString, sourceCode)
    val messages = KotlinCompiler.compile(tmpDir)
    !messages.hasErrors
  }
}
