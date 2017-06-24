package ruby.models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{ShouldMatchers, FunSpec}

class ResponsesWithUnitTypeSpec extends FunSpec with ShouldMatchers {

  private lazy val service = models.TestHelper.parseFile(s"/examples/response-with-unit-type.json")

  it("parses response type") {
    RubyClientGenerator.invoke(InvocationForm(service = service)) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/example-response-with-unit-type.txt", sourceFiles.head.contents)
      }
    }
  }

}
