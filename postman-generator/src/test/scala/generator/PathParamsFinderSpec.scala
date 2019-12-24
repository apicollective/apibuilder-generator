package generator

import org.scalatest.{Matchers, WordSpec}

class PathParamsFinderSpec extends WordSpec with Matchers {

  "PathParamsFinder" should {

    "find params in path" in {
      PathParamsFinder.find("/:organization/attributes/:key") shouldEqual List("organization", "key")
      PathParamsFinder.find("/:organization/attributes/:key/:abc") shouldEqual List("organization", "key", "abc")
    }

  }

}
