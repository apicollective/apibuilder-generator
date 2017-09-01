package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, ShouldMatchers}

import scala.generator.ScalaClientMethodConfigs
import scala.models.http4s.server.Http4sServer

class Http4sServerGeneratorSpec extends FunSpec with ShouldMatchers {

  it("generates expected code for http4s 0.15 server path parameters") {
    val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
                              new ScalaService(service),
                              new ScalaClientMethodConfigs.Http4s015(namespace = "whatever", baseUrl = None))
    models.TestHelper.assertEqualsFile("/http4s/path-params-015.txt", server.generate())
  }

  it("generates expected code for http4s 0.17 server path parameters") {
    val service = models.TestHelper.parseFile(s"/http4s/path-params.json")
    val server = Http4sServer(new InvocationForm(service, Seq.empty, None),
      new ScalaService(service),
      new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", baseUrl = None))
    models.TestHelper.assertEqualsFile("/http4s/path-params-017.txt", server.generate())
  }

}
