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

import org.scalatest.Matchers

object TestHelper extends Matchers {

  lazy val collectionJsonDefaultsService: Service = parseFile("/examples/collection-json-defaults.json")
  lazy val referenceApiService: Service = parseFile(s"/examples/reference-service.json")
  lazy val referenceWithImportsApiService: Service = parseFile(s"/examples/reference-with-imports.json")
  lazy val generatorApiService: Service = parseFile(s"/examples/apidoc-generator.json")
  lazy val apidocApiService: Service = parseFile(s"/examples/apidoc-api.json")

  lazy val generatorApiServiceWithUnionAndDescriminator: Service = parseFile(s"/examples/apidoc-example-union-types-discriminator.json")
  lazy val generatorApiServiceWithUnionWithoutDescriminator: Service = parseFile(s"/examples/apidoc-example-union-types.json")

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
  }

  def readFile(path: String): String = {
    scala.io.Source.fromFile(new JFile(path)).getLines.mkString("\n")
  }

  def parseFile(path: String): Service = {
    service(readFile(resolvePath(path)))
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
      case Right(sourceTree) => { /* cool, we have valid source code */ }
    }
  }

  def assertEqualsFile(filename: String, contents: String): Unit = {
    val actualPath = resolvePath(filename)
    val current = readFile(actualPath).trim
    if (current != contents.trim) {
      import sys.process._

      val expectedPath = "/tmp/apidoc.tmp.expected." + Text.safeName(filename)
      TestHelper.writeToFile(expectedPath, contents.trim)
      // TestHelper.writeToFile(actualPath, contents.trim)
      
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
