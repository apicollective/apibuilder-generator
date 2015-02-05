package models

import com.gilt.apidoc.generator.v0.models.InvocationForm
import org.scalatest.{ShouldMatchers, FunSpec}

class ExampleUnionTypesSpec extends FunSpec with ShouldMatchers {

  val service = TestHelper.parseFile(s"../example-union-types/api/service.json")

  it("generates expected code") {
    val code = Play23ClientGenerator.invoke(InvocationForm(service = service))
    TestHelper.assertEqualsFile("test/resources/example-union-types.txt", code)
  }

}
