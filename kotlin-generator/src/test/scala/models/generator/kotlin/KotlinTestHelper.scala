package models.generator.kotlin

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Service
import io.github.sullis.kotlin.compiler.KotlinCompiler
import models.TestHelper.{assertJodaTimeNotPresent, writeFiles}
import org.scalatest.matchers.should.Matchers

object KotlinTestHelper extends Matchers {

  def generateSourceFiles(service: Service): java.io.File = {
    val tmpDir = createTempDirectory(service)
    val invocationForm = InvocationForm(service, Seq.empty, None)
    val generator = new KotlinGenerator()
    val files = generator.invoke(invocationForm).getOrElse(sys.error("got Left"))
    files.size shouldBe >(0)
    files.foreach(f => {
      f.contents.length shouldBe >(0)
      f.name should endWith(".kt")
    })
    assertFileExists("JacksonObjectMapperFactory.kt", files)

    service.enums.foreach( e => {
      val expectedFilename = KotlinUtil.capitalizeModelName(e.name) + ".kt"
      assertFileExists(expectedFilename, files)
      val file = getFile(expectedFilename, files)
      file.dir.get should endWith ("/enums")
    })

    service.unions.foreach( u => {
      val expectedFilename = KotlinUtil.capitalizeModelName(u.name) + ".kt"
      assertFileExists(expectedFilename, files)
      val file = getFile(expectedFilename, files)
      file.dir.get should endWith ("/models")
    })

    service.models.foreach( m => {
      val expectedFilename = KotlinUtil.capitalizeModelName(m.name) + ".kt"
      assertFileExists(expectedFilename, files)
      val file = getFile(expectedFilename, files)
      file.dir.get should endWith ("/models")
      m.fields.foreach( f => {
        if (KotlinUtil.isParameterArray(f.`type`)) {
          assertFileContainsString("import kotlin.collections.List", file)
        }
      })
    })

    assertJodaTimeNotPresent(files)
    writeFiles(tmpDir, files)
    tmpDir
  }

  def assertFileExists(filename: String, files: Seq[File]): Unit = {
    files.exists(
      file => (file.name == filename)
    ) shouldBe true
    ()
  }

  def assertPackageExists(packageName: String, files: Seq[File]): Unit = {
    assertFileContainsString(s"package ${packageName}", files)
  }

  def assertFileContainsString(s: String, files: Seq[File]): Unit = {
    files.exists(
      file => file.contents.contains(s)
    ) shouldBe true
    ()
  }

  def assertFileContainsString(s: String, file: File): Unit = {
    file.contents.contains(s) shouldBe true
    ()
  }

  def getFile(filename: String, files: Seq[File]): File = {
    files.filter(_.name == filename).head
  }

  def assertKotlinCodeCompiles(kotlinSourceDirectory: java.io.File): Unit = {
    assert(kotlinSourceDirectory.exists())
    assert(kotlinSourceDirectory.canRead)
    assert(kotlinSourceDirectory.isDirectory)
    val result = new KotlinCompiler().compileSourceDir(kotlinSourceDirectory.toPath)
    result.isSuccess shouldBe true
    ()
  }

  private def createTempDirectory(service: Service): java.io.File = {
    val name = (service.name + System.currentTimeMillis).replace(' ', '-')
    val tmpdir = "/tmp"
    val dir = new java.io.File(tmpdir + "/" + name)
    dir.mkdirs()
    dir.deleteOnExit()
    dir
  }
}
