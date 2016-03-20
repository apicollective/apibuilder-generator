package ruby.models

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import org.scalatest.{ShouldMatchers, FunSpec}

class ExampleUnionTypesSpec extends FunSpec with ShouldMatchers {

  private lazy val service = models.TestHelper.parseFile(s"/examples/apidoc-example-union-types.json")

  it("generates expected code for ruby client") {
    RubyClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/example-union-types-ruby-client.txt", sourceFiles.head.contents)
      }
    }
  }

}
