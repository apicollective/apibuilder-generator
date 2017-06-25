
package scala.models.http4s

import models.TestHelper.assertValidScalaSourceCode
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, Matchers}
import scala.generator.ScalaClientMethodConfigs

class Http4sClientGeneratorSpec extends FunSpec with Matchers {

  describe("generate") {
    it("produces valid Scala source code") {
      val service = models.TestHelper.generatorApiService
      val ssd = new ScalaService(service)
      val invocationForm = new InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = new ScalaClientMethodConfigs.Http4s(namespace = "whatever", baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
    }
  }
}
