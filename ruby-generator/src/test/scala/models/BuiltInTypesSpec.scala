package ruby.models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BuiltInTypesSpec extends AnyFunSpec with Matchers {

  private lazy val service = models.TestHelper.parseFile(s"/examples/built-in-types.json")

  it("generates built-in types") {
    RubyClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/generators/ruby-built-in-types.txt", sourceFiles.head.contents)
      }
    }
  }

}
