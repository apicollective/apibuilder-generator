package models.generator.kotlin

import java.io.FileWriter
import java.util.concurrent.atomic.AtomicInteger

import io.apibuilder.generator.v0.models.{File => ApiBuilderFile}
import org.jetbrains.kotlin.cli.common.messages.{CompilerMessageLocation, CompilerMessageSeverity, MessageCollector}
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.jetbrains.kotlin.config.Services
import org.scalatest.Matchers

object KotlinTestHelper extends Matchers {

  def writeFiles(dir: java.io.File, files: Seq[ApiBuilderFile]): Unit = {
    dir.createNewFile()
    for (f <- files) {
      val javaFile = new java.io.File(dir.getAbsolutePath, f.name)
      javaFile.createNewFile()
      val writer = new FileWriter(javaFile)
      writer.write(f.contents)
      writer.flush()
      writer.close()
    }
  }

  def assertValidKotlinSourceCode(sourceDirectory: java.nio.file.Path): Unit = {
    System.out.println("Kotlin: sourceDirectory=" + sourceDirectory.toString)
    val freeArgList = new java.util.ArrayList[String]
    freeArgList.add(sourceDirectory.toString)
    val msgCollector = new MessageCollectorImpl()
    val compiler = new K2JVMCompiler()
    val args = new K2JVMCompilerArguments()
    args.setFreeArgs(freeArgList)
    args.setClasspath(System.getProperty("java.class.path"))
    args.setCompileJava(true)
    val exitCode = compiler.exec(msgCollector, Services.EMPTY, args)
    msgCollector.hasErrors shouldBe false
  }

  class MessageCollectorImpl extends MessageCollector {
    private val errorCount = new AtomicInteger(0)
    override def hasErrors: Boolean = { errorCount.get > 0 }
    override def clear(): Unit = ???
    override def report(severity: CompilerMessageSeverity, message: String, location: CompilerMessageLocation): Unit = {
      if (severity.isError) {
        errorCount.incrementAndGet()
        System.err.println("KotlinTestHelper ERROR: " + message)
      }
    }

  }
}
