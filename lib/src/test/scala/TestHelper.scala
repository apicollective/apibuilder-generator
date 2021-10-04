package models

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.{File => JFile}
import play.api.libs.json._
import io.apibuilder.spec.v0.models.{ResponseCode, ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.generator.v0.models.File
import lib.Text
import org.scalatest.matchers.should.Matchers

import scala.util.{Try, Failure, Success}

object TestHelper extends Matchers {

  lazy val collectionJsonDefaultsService: Service = parseFile("/examples/collection-json-defaults.json")
  lazy val referenceApiService: Service = parseFile("/examples/reference-service.json")
  lazy val referenceWithImportsApiService: Service = parseFile("/examples/reference-with-imports.json")
  lazy val generatorApiService: Service = parseFile("/examples/apidoc-generator.json")
  lazy val apidocApiService: Service = parseFile("/examples/apidoc-api.json")
  lazy val dateTimeService: Service = parseFile("/examples/date-time-types.json")
  lazy val builtInTypesService: Service = parseFile("/examples/built-in-types.json")
  lazy val scalaKeywordsService: Service = parseFile("/examples/response-with-reserved-scala-keyword.json")
  lazy val statusCodesService: Service = parseFile("/http4s/server/status-codes.json")

  lazy val generatorApiServiceWithUnionAndDiscriminator: Service = parseFile("/examples/apidoc-example-union-types-discriminator.json")
  lazy val generatorApiServiceWithUnionWithoutDiscriminator: Service = parseFile("/examples/apidoc-example-union-types.json")

  lazy val emptyService: Service = service(buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "models": [],
      "enums": [],
      "unions": [],
      "models": [],
      "resources": [],
      "attributes": []
    """))

  def buildJson(json: String): String = {
    val specVersion = io.apibuilder.spec.v0.Constants.Version
    val body = s"""
      "apidoc": { "version": "$specVersion" },
      "base_url": "http://localhost:9000",
      "name": "API Builder Test",
      "organization": { "key": "test" },
      "application": { "key": "apidoc-test" },
      "namespace": "test.apidoc.apidoctest.v0",
      "version": "1.0.0"
    """

    if (json.isEmpty) {
      s"{ $body } "
    } else {
      s"{ $body, $json } "
    }
  }

  def writeToFile(path: String, contents: String): Unit = {
    val outputPath = Paths.get(path)
    val bytes = contents.getBytes(StandardCharsets.UTF_8)
    Files.write(outputPath, bytes)
    ()
  }

  def readFile(path: String): String = {
    readFile(new JFile(path))
  }

  def readFile(path: JFile): String = {
    val source = scala.io.Source.fromFile(path)
    try {
      Try {
        source.getLines().mkString("\n")
      } match {
        case Success(contents) => contents
        case Failure(ex) => {
          sys.error(s"Failed to read file '${path.getAbsolutePath}: ${ex.getMessage}")
        }
      }
    } finally {
      source.close()
    }
  }

  def parseFile(path: String): Service = {
    service(readFile(resolvePath(path)))
  }

  def writeFiles(dir: java.io.File, files: Seq[File]): Unit = {
    dir.createNewFile()
    for (f <- files) {
      val javaFile = new java.io.File(dir.getAbsolutePath, f.name)
      javaFile.createNewFile()
      val writer = new java.io.FileWriter(javaFile)
      writer.write(f.contents)
      writer.flush()
      writer.close()
    }
  }

  /**
    * Finds the actual source file for the resource with this name. We
    * need this to easily overwrite the resource when updating our
    * generated code.
    */
  private[this] def resolvePath(filename: String): String = {
    import sys.process._

    val targetPath = Option(getClass.getResource(filename)).map(r => new JFile(r.getPath)).getOrElse {
      sys.error(s"Could not find file named[$filename]")
    }
    val cmd = s"find . -type f -name ${targetPath.getName}"

    cmd.!!.trim.split("\\s+").toSeq.filter(_.indexOf("/target") < 0).filter(_.endsWith(filename)).toList match {
      case Nil => sys.error(s"Could not find source file named[$filename]")
      case one :: Nil => one
      case multiple => sys.error(s"Multiple source files named[$filename]: " + multiple.mkString(", "))
    }
  }

  def assertJodaTimeNotPresent(files: Seq[File]): Unit = {
    files.foreach(assertJodaTimeNotPresent)
  }

  def assertJodaTimeNotPresent(file: File): Unit = {
    file.contents shouldNot include("org.joda.time")
    ()
  }

  def assertValidScalaSourceFiles(files: Seq[File]): Unit = {
    files.length shouldBe > (0)
    files.foreach(assertValidScalaSourceFile)
  }

  def assertValidScalaSourceFile(file: File): Unit = {
    file.name.length shouldBe > (0)
    assertValidScalaSourceCode(file.contents, Some(file.name))
  }

  def assertValidScalaSourceCode(scalaSourceCode: String, filename: Option[String] = None): Unit = {
    import scala.meta._

    val virtualFile = Input.VirtualFile(filename.getOrElse("filename.scala"), scalaSourceCode)
    val parseResult = virtualFile.parse[Source]
    parseResult.toEither match {
      case Left(parseError) => fail(s"Not valid Scala source. ${parseError.toString()}")
      case Right(_) => /* cool, we have valid source code */
    }
  }

  /**
   * If you want to rewrite the tests:
   *   touch /tmp/apibuilder.generator.overwritetests.tmp
   *   sbt test
   */
  private[this] val OverwriteTestsFile = Paths.get("/tmp/apibuilder.generator.overwritetests.tmp")

  def assertEqualsFile(filename: String, contents: String): Unit = {
    val actualPath = resolvePath(filename)
    val current = readFile(actualPath).trim
    if (current != contents.trim) {
      import sys.process._

      val expectedPath = "/tmp/apidoc.tmp.expected." + Text.safeName(filename)
      TestHelper.writeToFile(expectedPath, contents.trim)
      if (Files.exists(OverwriteTestsFile)) {
        println(s"Overwriting test output as File $OverwriteTestsFile exists")
        TestHelper.writeToFile(actualPath, contents.trim)
      } else {
        println("If you would like to update the test output automatically")
        println(s"  1. touch $OverwriteTestsFile")
        println("  2. rerun the tests")
      }

      val cmd = s"diff $expectedPath $actualPath"
      println(cmd)
      cmd.!
      sys.error(s"Test output did not match. $cmd")
    }
  }

  def service(json: String): Service = {
    Json.parse(json).validate[Service] match {
      case e: JsError => sys.error("Failed to parse json: " + e)
      case s: JsSuccess[Service] => s.get
    }
  }

  def responseCode(responseCode: ResponseCode): String = {
    responseCode match {
      case ResponseCodeInt(v) => v.toString
      case ResponseCodeOption.Default => ResponseCodeOption.Default.toString
      case ResponseCodeOption.UNDEFINED(v) => sys.error(s"invalid value[$v]")
      case ResponseCodeUndefinedType(v) => sys.error(s"invalid response code type[$v]")
    }
  }

}
