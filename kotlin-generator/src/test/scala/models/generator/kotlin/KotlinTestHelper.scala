package models.generator.kotlin

import java.util.concurrent.atomic.AtomicInteger
import org.jetbrains.kotlin.cli.common.messages.{CompilerMessageLocation, CompilerMessageSeverity, MessageCollector}
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.jetbrains.kotlin.config.Services
import org.scalatest.Matchers

object KotlinTestHelper extends Matchers {

  def assertValidKotlinSourceCode(kotlinSourceDirectory: java.nio.file.Path): Unit = {
    System.out.println("kotlinSourceDirectory=" + kotlinSourceDirectory.toString)
    val freeArgList = new java.util.ArrayList[String]
    freeArgList.add(kotlinSourceDirectory.toString)
    val msgCollector = new MessageCollectorImpl()
    val compiler = new K2JVMCompiler()
    val args = new K2JVMCompilerArguments()
    args.setFreeArgs(freeArgList)
    val classpath = System.getProperty("java.class.path")
    args.setClasspath(classpath)
    args.setCompileJava(true)
    args.setNoStdlib(true)
    args.setNoReflect(true)
    val exitCode = compiler.exec(msgCollector, Services.EMPTY, args)
    System.out.println("KotlinCompiler: exitCode=" + exitCode)
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
