package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.{FunSpec, ShouldMatchers}

import scala.generator.ScalaClientMethodConfigs
import scala.models.http4s.server.Http4sServer

class Http4sServerGeneratorSpec extends FunSpec with ShouldMatchers {

  it("path parameters test generates expected code for http4s 0.15 server") {
    val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
                              new ScalaService(service),
                              new ScalaClientMethodConfigs.Http4s015(namespace = "whatever", baseUrl = None))
    val scalaSourceCode = server.generate()
    assertValidScalaSourceCode(scalaSourceCode)
    models.TestHelper.assertEqualsFile("/http4s/path-params-015.txt", scalaSourceCode)
  }

  it("path parameters test generates expected code for http4s 0.17 server") {
    val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
      new ScalaService(service),
      new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
    val scalaSourceCode = server.generate()
    assertValidScalaSourceCode(scalaSourceCode)
    models.TestHelper.assertEqualsFile("/http4s/path-params-017.txt", scalaSourceCode)
  }

  it("query parameters test generates expected code for http4s 0.17 server") {
    val service = models.TestHelper.parseFile(s"/http4s/query-params.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
      new ScalaService(service),
      new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
    val scalaSourceCode = server.generate()
    assertValidScalaSourceCode(scalaSourceCode)
    models.TestHelper.assertEqualsFile("/http4s/query-params-017.txt", scalaSourceCode)
  }

  it("response types test generates expected code for http4s 0.17 server") {
    val service = models.TestHelper.parseFile(s"/http4s/response-types.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
      new ScalaService(service),
      new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
    val scalaSourceCode = server.generate()
    assertValidScalaSourceCode(scalaSourceCode)
    models.TestHelper.assertEqualsFile("/http4s/response-types-017.txt", scalaSourceCode)
  }

  it("imported types test generates expected code for http4s 0.17 server") {
    val service = models.TestHelper.parseFile(s"/http4s/imported-types.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
      new ScalaService(service),
      new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
    val scalaSourceCode = server.generate()
    assertValidScalaSourceCode(scalaSourceCode)
    models.TestHelper.assertEqualsFile("/http4s/imported-types-017.txt", scalaSourceCode)
  }
}
