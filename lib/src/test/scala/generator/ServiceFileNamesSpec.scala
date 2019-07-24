package generator

import io.apibuilder.generator.v0.models.File
import org.scalatest.{FunSpec, Matchers}

class ServiceFileNamesSpec extends FunSpec with Matchers {

  describe("ServiceFileNames.toFile") {

    def toFile(
      languages: String,
      version: String = "0.0.1"
    ): File = {
      ServiceFileNames.toFile(
        namespace = "io.apibuilder",
        organizationKey = "apicollective",
        applicationKey = "apibuilder",
        version = version,
        suffix = "Client",
        contents = "test",
        languages = Some(languages)
      )
    }

    it("ruby is underscored") {
      val file = toFile("ruby")
      file.name should be("apicollective_apibuilder_v0_client.rb")
      file.dir should be(Some("io/apibuilder"))
    }

    it("scala is camelcased") {
      val file = toFile("scala")
      file.name should be("ApicollectiveApibuilderV0Client.scala")
      file.dir should be(Some("io/apibuilder"))
    }

  }
  
  it("getSuffix for known languages") {
    ServiceFileNames.toLanguages("ruby").map(_.extension) should be(Seq("rb"))
    ServiceFileNames.toLanguages("ruby,scala").map(_.extension) should be(Seq("rb", "scala"))
    ServiceFileNames.toLanguages(" RUBY , SCALA ").map(_.extension) should be(Seq("rb", "scala"))
    ServiceFileNames.toLanguages("java").map(_.extension) should be(Seq("java"))
    ServiceFileNames.toLanguages("javascript").map(_.extension) should be(Seq("js"))
    ServiceFileNames.toLanguages("go").map(_.extension) should be(Seq("go"))
    ServiceFileNames.toLanguages("kotlin").map(_.extension) should be(Seq("kt"))
  }

  it("getSuffix for all known languages") {
    ServiceFileNames.Language.All.foreach { l =>
      ServiceFileNames.toLanguages(l.name).map(_.extension) should be(Seq(l.extension))
    }
  }

  it("getSuffix for unknown languages") {
    ServiceFileNames.toLanguages("") should be(Nil)
    ServiceFileNames.toLanguages("foo") should be(Nil)
    ServiceFileNames.toLanguages("foo, bar") should be(Nil)
  }

}
