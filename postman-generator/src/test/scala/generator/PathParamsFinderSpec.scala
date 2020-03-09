package generator

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PathParamsFinderSpec extends AnyWordSpec with Matchers {

  "PathParamsFinder" should {

    "find params in path" in {
      PathParamsFinder.find("/:organization/attributes/:key") shouldEqual List("organization", "key")
      PathParamsFinder.find("/:organization/attributes/:key/:abc") shouldEqual List("organization", "key", "abc")
    }

  }

}
