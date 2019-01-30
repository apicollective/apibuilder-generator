package generator

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.WordSpec

class PostmanCollectionGeneratorSpec extends WordSpec {

  import models.TestHelper._

  "PostmanCollectionGenerator" should {

    "return errors when it's not ready" in {
      val invocationForm = InvocationForm(referenceApiService)

      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isLeft shouldEqual true
    }

  }

}
