package lib

import org.scalatest.{FunSpec, ShouldMatchers}

class ServiceFileNamesSpec extends FunSpec with ShouldMatchers {

  it("getSuffix for known languages") {
    ServiceFileNames.toLanguages("ruby").map(_.extension) should be(Seq("rb"))
    ServiceFileNames.toLanguages("ruby,scala").map(_.extension) should be(Seq("rb", "scala"))
    ServiceFileNames.toLanguages(" RUBY , SCALA ").map(_.extension) should be(Seq("rb", "scala"))
    ServiceFileNames.toLanguages("java").map(_.extension) should be(Seq("java"))
    ServiceFileNames.toLanguages("javascript").map(_.extension) should be(Seq("js"))
    ServiceFileNames.toLanguages("go").map(_.extension) should be(Seq("go"))
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
