package models

import com.gilt.apidoc.generator.v0.models.InvocationForm
import ning.Ning18ClientGenerator
import org.scalatest.{ShouldMatchers, FunSpec}

class ExampleUnionTypesSpec extends FunSpec with ShouldMatchers {

  val service = TestHelper.parseFile(s"../example-union-types/api/service.json")

  it("generates expected code for play 2.3 client") {
    val code = Play23ClientGenerator.invoke(InvocationForm(service = service))
    TestHelper.assertEqualsFile("test/resources/example-union-types-play-23.txt", code)
  }

  it("generates expected code for ning client") {
    val code = Ning18ClientGenerator.invoke(InvocationForm(service = service))
    TestHelper.assertEqualsFile("test/resources/example-union-types-ning-client.txt", code)
  }

  it("generates expected code for ruby client") {
    val code = RubyClientGenerator.invoke(InvocationForm(service = service))
    TestHelper.assertEqualsFile("test/resources/example-union-types-ruby-client.txt", code)
  }

}
