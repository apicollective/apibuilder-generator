package models

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

import play.api.libs.json._
import com.gilt.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCode, ResponseCodeOption, ResponseCodeUndefinedType}
import com.gilt.apidoc.spec.v0.models.json._
import com.gilt.apidoc.spec.v0.models.Service
import lib.Text
import java.io.File

object TestHelper {

  lazy val collectionJsonDefaultsService = parseFile("/examples/collection-json-defaults.json")
  lazy val illegalNonRequiredWithDefaultService = parseFile("/examples/illegal-non-required-with-default.json")
  lazy val referenceApiService = parseFile(s"/examples/reference-service.json")
  lazy val referenceWithImportsApiService = parseFile(s"/examples/reference-with-imports.json")
  lazy val generatorApiService = parseFile(s"/examples/apidoc-generator.json")
  lazy val apidocApiService = parseFile(s"/examples/apidoc-api.json")

  def buildJson(json: String): String = {
    val specVersion = com.gilt.apidoc.spec.v0.Constants.Version
    val body = s"""
      "apidoc": { "version": "$specVersion" },
      "base_url": "http://localhost:9000",
      "name": "Api Doc Test",
      "organization": { "key": "test" },
      "application": { "key": "apidoc-test" },
      "namespace": "test.apidoc.apidoctest.v0",
      "version": "1.0.0"
    """

    json.isEmpty match {
      case true => s"{ $body } "
      case false => s"{ $body, $json } "
    }
  }

  def writeToFile(path: String, contents: String) {
    val outputPath = Paths.get(path)
    val bytes = contents.getBytes(StandardCharsets.UTF_8)
    Files.write(outputPath, bytes)
  }

  def readFile(path: String): String = {
    val stream = getClass.getResourceAsStream(path);
    if (stream == null) sys.error(s"unable to find resource at ${path}")
    scala.io.Source.fromInputStream(stream)
      .getLines.mkString("\n")
  }

  def parseFile(path: String): Service = {
    service(readFile(path))
  }

  def assertEqualsFile(filename: String, contents: String) {
    val expected = readFile(filename).trim
    if (contents.trim != expected) {
      val actualPath = "/tmp/apidoc.tmp." + Text.safeName(filename)
      val expectedPath = "/tmp/apidoc.tmp.expected." + Text.safeName(filename)
      TestHelper.writeToFile(actualPath, contents.trim)
      TestHelper.writeToFile(expectedPath, expected)
      // TestHelper.writeToFile(filename, contents.trim)
      import sys.process._
      val cmd = s"diff $actualPath $expectedPath"
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
      case ResponseCodeInt(value) => value.toString
      case ResponseCodeOption.Default => ResponseCodeOption.Default.toString
      case ResponseCodeOption.UNDEFINED(value) => sys.error(s"invalid value[$value]")
      case ResponseCodeUndefinedType(value) => sys.error(s"invalid response code type[$value]")
    }
  }

}
