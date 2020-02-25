
package scala.models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertValidScalaSourceCode
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.generator.ScalaClientMethodConfigs
import scala.models.Attributes

class Http4sClientGeneratorSpec extends AnyFunSpec with Matchers {

  describe("generate") {
    it("Http4s 0.15 generator produces valid Scala source code") {
      val service = models.TestHelper.generatorApiService
      val ssd = new ScalaService(service)
      val invocationForm = InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = ScalaClientMethodConfigs.Http4s015(namespace = "whatever", attributes = Attributes.Http4sDefaultConfig, baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      scalaSourceCode should not include ("org.joda")
      scalaSourceCode should include ("scalaz.concurrent.Task")
      scalaSourceCode should include (" scalaz.-\\/")
      scalaSourceCode should include (" scalaz.\\/-")
      scalaSourceCode should not include ("fs2.Task")
      scalaSourceCode should not include ("cats.effect.IO")
      scalaSourceCode should not include (" Left")
      scalaSourceCode should not include (" Right")
      scalaSourceCode should not include ("Response[F]")
      scalaSourceCode should not include ("org.http4s.circe.jsonOf[F,")
      scalaSourceCode should not include ("org.http4s.circe.jsonEncoderOf[F,")
    }

    it("Http4s 0.17 generator produces valid Scala source code") {
      val service = models.TestHelper.generatorApiService
      val ssd = new ScalaService(service)
      val invocationForm = new InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = new ScalaClientMethodConfigs.Http4s017(namespace = "whatever", attributes = Attributes.Http4sDefaultConfig, baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      scalaSourceCode should not include ("org.joda")
      scalaSourceCode should not include ("scalaz.concurrent.Task")
      scalaSourceCode should not include ("-\\/")
      scalaSourceCode should not include ("\\/-")
      scalaSourceCode should not include ("cats.effect.IO")
      scalaSourceCode should include ("fs2.Task")
      scalaSourceCode should include (" Left")
      scalaSourceCode should include (" Right")
      scalaSourceCode should not include ("Response[F]")
      scalaSourceCode should not include ("org.http4s.circe.jsonOf[F,")
      scalaSourceCode should not include ("org.http4s.circe.jsonEncoderOf[F,")
    }

    it("Http4s 0.18 generator produces valid Scala source code") {
      val service = models.TestHelper.generatorApiService
      val ssd = new ScalaService(service)
      val invocationForm = new InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", attributes = Attributes.Http4sDefaultConfig, baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      scalaSourceCode should not include ("org.joda")
      scalaSourceCode should not include ("scalaz.concurrent.Task")
      scalaSourceCode should not include ("-\\/")
      scalaSourceCode should not include ("\\/-")
      scalaSourceCode should not include ("fs2.Task")
      scalaSourceCode should not include ("cats.effect.IO")
      scalaSourceCode should include ("Response[F]")
      scalaSourceCode should include ("import scala.language.higherKinds")
      scalaSourceCode should include ("org.http4s.circe.jsonOf[F,")
      scalaSourceCode should include ("org.http4s.circe.jsonEncoderOf[F,")
      scalaSourceCode should include (" Left")
      scalaSourceCode should include (" Right")
      scalaSourceCode should include (".raiseError")
      scalaSourceCode should include (".pure")
    }

    it("Http4s 0.20 generator produces valid Scala source code") {
      val service = models.TestHelper.generatorApiService
      val ssd = new ScalaService(service)
      val invocationForm = new InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", attributes = Attributes.Http4sDefaultConfig, baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      scalaSourceCode should not include ("org.joda")
      scalaSourceCode should not include ("scalaz.concurrent.Task")
      scalaSourceCode should not include ("-\\/")
      scalaSourceCode should not include ("\\/-")
      scalaSourceCode should not include ("fs2.Task")
      scalaSourceCode should not include ("cats.effect.IO")
      scalaSourceCode should include ("Response[F]")
      scalaSourceCode should include ("org.http4s.circe.jsonOf[F,")
      scalaSourceCode should include ("org.http4s.circe.jsonEncoderOf[F,")
      scalaSourceCode should include (" Left")
      scalaSourceCode should include (" Right")
      scalaSourceCode should include (".raiseError")
      scalaSourceCode should include (".pure")
    }

    it("Circe generator handles primitive union types") {
      val service = models.TestHelper.generatorApiServiceWithUnionAndDescriminator
      val ssd = new ScalaService(service)
      val json = CirceJson(ssd)
      val scalaSourceCode = json.generate()
      assertValidScalaSourceCode(scalaSourceCode)

      scalaSourceCode should include ("decodeApidocExampleUnionTypesDiscriminatorUserString")
      scalaSourceCode should include ("encodeApidocExampleUnionTypesDiscriminatorUserString")
    }

    it("Circe generator produces valid json decoder for unions without descriminator") {
      val service = models.TestHelper.generatorApiServiceWithUnionWithoutDescriminator
      val ssd = new ScalaService(service)
      val json = CirceJson(ssd)
      val scalaSourceCode = json.generate()
      assertValidScalaSourceCode(scalaSourceCode)

      scalaSourceCode should not include ("import cats.implicits._")
    }

    it("Http4s 0.18 generator produces valid url form marshalling code") {
      val service = models.TestHelper.referenceApiService
      val ssd = new ScalaService(service)
      val invocationForm = new InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = new ScalaClientMethodConfigs.Http4s018(namespace = "whatever", attributes = Attributes.Http4sDefaultConfig, baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      scalaSourceCode should include ("formBody = formPayload,")
      scalaSourceCode should include (""""active" -> active.asJson.noSpaces""")
    }

    it("Http4s 0.20 generator produces valid url form marshalling code") {
      val service = models.TestHelper.referenceApiService
      val ssd = new ScalaService(service)
      val invocationForm = new InvocationForm(service, Seq.empty, None)
      val clientMethodConfig = new ScalaClientMethodConfigs.Http4s020(namespace = "whatever", attributes = Attributes.Http4sDefaultConfig, baseUrl = None)
      val client = Http4sClient(invocationForm, ssd, clientMethodConfig)
      val scalaSourceCode = client.generate()
      assertValidScalaSourceCode(scalaSourceCode)
      scalaSourceCode should include ("formBody = formPayload,")
      scalaSourceCode should include (""""active" -> active.asJson.noSpaces""")
    }
  }
}
