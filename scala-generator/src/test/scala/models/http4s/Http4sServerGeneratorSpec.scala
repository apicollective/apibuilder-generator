package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.{FunSpec, Matchers}

import scala.generator.ScalaClientMethodConfigs
import scala.models.http4s.server.Http4sServer

class Http4sServerGeneratorSpec extends FunSpec with Matchers {
  describe("path parameters") {
    it("http4s 0.15 server") {
      val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s015(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/path-params-015.txt", scalaSourceCode)
    }

    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/path-params-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/path-params-018.txt", scalaSourceCode)
    }
  }

  describe("query parameters") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/query-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/query-params-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/query-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/query-params-018.txt", scalaSourceCode)
    }
  }

  describe("form parameters") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/form-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/form-params-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/form-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/form-params-018.txt", scalaSourceCode)
    }
  }

  describe("response types") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/response-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/response-types-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/response-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/response-types-018.txt", scalaSourceCode)
    }
  }

  describe("imported types") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/imported-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/imported-types-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/imported-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/imported-types-018.txt", scalaSourceCode)
    }
  }

  describe("status codes types") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/status-codes.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/status-codes-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/status-codes.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/status-codes-018.txt", scalaSourceCode)
    }
  }
}
