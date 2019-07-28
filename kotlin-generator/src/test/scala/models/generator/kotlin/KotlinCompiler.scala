package models.generator.kotlin

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger
import org.jetbrains.kotlin.cli.common.ExitCode
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments
import org.jetbrains.kotlin.cli.common.messages.{CompilerMessageLocation, CompilerMessageSeverity, MessageCollector}
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.jetbrains.kotlin.config.Services

object KotlinCompiler {

  def compile(kotlinSourceDirectory: Path): MessageCollector = {
    val compilerOutputDir = new java.io.File(kotlinSourceDirectory.toAbsolutePath.toString + "-output");
    compilerOutputDir.mkdirs()
    compilerOutputDir.deleteOnExit()

    // System.out.println("kotlinSourceDirectory=" + kotlinSourceDirectory.toString)
    // System.out.println("compilerOutputDir=" + compilerOutputDir.toString)

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
    args.setDestination(compilerOutputDir.getAbsolutePath)
    val exitCode = compiler.exec(msgCollector, Services.EMPTY, args)
    if (exitCode != ExitCode.OK) {
      System.err.println("KotlinCompiler: exitCode=" + exitCode)
    }
    msgCollector
  }

  private class MessageCollectorImpl extends MessageCollector {
    private val errorCount = new AtomicInteger(0)
    override def hasErrors: Boolean = { errorCount.get > 0 }
    override def clear(): Unit = ???
    override def report(severity: CompilerMessageSeverity, message: String, location: CompilerMessageLocation): Unit = {
      if (severity.isError) {
        errorCount.incrementAndGet()
        System.err.println("KotlinTestHelper ERROR: " + message + " " + location)
      }
    }

  }
}
