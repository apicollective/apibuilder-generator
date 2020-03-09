package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode

import scala.generator.ScalaClientMethodConfigs
import scala.models.Attributes
import scala.models.http4s.server.Http4sServer
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Http4sServerGeneratorSpec extends AnyFunSpec with Matchers {
  describe("path parameters") {
    it("http4s 0.15 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s015(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/path-params-015.txt", scalaSourceCode)
    }

    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/path-params-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/path-params-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/path-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/path-params-020.txt", scalaSourceCode)
    }
  }

  describe("query parameters") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/query-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/query-params-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/query-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/query-params-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/query-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/query-params-020.txt", scalaSourceCode)
    }
  }

  describe("form parameters") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/form-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/form-params-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/form-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/form-params-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/form-params.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/form-params-020.txt", scalaSourceCode)
    }
  }

  describe("response types") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/response-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/response-types-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/response-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/response-types-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/response-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/response-types-020.txt", scalaSourceCode)
    }
  }

  describe("imported types") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/imported-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/imported-types-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/imported-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/imported-types-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/imported-types.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/imported-types-020.txt", scalaSourceCode)
    }
  }

  describe("status codes types") {
    it("http4s 0.17 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/status-codes.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/status-codes-017.txt", scalaSourceCode)
    }

    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/status-codes.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/status-codes-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/status-codes.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/status-codes-020.txt", scalaSourceCode)
    }
  }

  describe("snake and kebab case fields and params") {
    it("http4s 0.18 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/kebab-and-snake-case.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/kebab-and-snake-case-018.txt", scalaSourceCode)
    }

    it("http4s 0.20 server") {
      val service = models.TestHelper.parseFile(s"/http4s/server/kebab-and-snake-case.json")
      val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
        new ScalaService(service),
        new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", Attributes.Http4sDefaultConfig, baseUrl = None))
      val scalaSourceCode = server.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      models.TestHelper.assertEqualsFile("/http4s/server/kebab-and-snake-case-020.txt", scalaSourceCode)
    }
  }
}
