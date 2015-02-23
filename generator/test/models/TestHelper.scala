package models

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

import play.api.libs.json._
import com.gilt.apidoc.spec.v0.models.json._
import com.gilt.apidoc.spec.v0.models.Service
import generator.ScalaService
import lib.Text
import java.io.File

object TestHelper {

  lazy val referenceApiService = parseFile(s"../reference-api/service.json")
  lazy val generatorApiService = parseFile(s"test/resources/examples/generator.json")
  lazy val apidocApiService = parseFile(s"test/resources/examples/apidoc.json")

  def buildJson(json: String): String = {
    s"""
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc Test",
      "organization": { "key": "test" },
      "application": { "key": "apidoc-test" },
      "namespace": "test.apidoc.apidoctest.v0",
      "version": "1.0.0",

      $json
    }
    """
  }

  def writeToFile(path: String, contents: String) {
    val outputPath = Paths.get(path)
    val bytes = contents.getBytes(StandardCharsets.UTF_8)
    Files.write(outputPath, bytes)
  }

  def readFile(path: String): String = {
    if ((new File(path)).exists()) {
      scala.io.Source.fromFile(path).getLines.mkString("\n")
    } else {
      ""
    }
  }

  def parseFile(filename: String): Service = {
    service(readFile(filename))
  }

  def assertEqualsFile(filename: String, contents: String) {
    if (contents.trim != readFile(filename).trim) {
      val tmpPath = "/tmp/apidoc.tmp." + Text.safeName(filename)
      TestHelper.writeToFile(tmpPath, contents.trim)
      // TestHelper.writeToFile(filename, contents.trim)
      sys.error(s"Test output did not match. diff $tmpPath $filename")
    }
  }

  def service(json: String): Service = {
    Json.parse(json).validate[Service] match {
      case e: JsError => sys.error("Failed to parse json: " + e)
      case s: JsSuccess[Service] => s.get
    }
  }

  def scalaService(json: String): ScalaService = {
    ScalaService(service(json))
  }

}
