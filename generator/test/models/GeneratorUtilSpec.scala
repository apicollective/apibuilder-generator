package models

import lib.{Datatype, Primitives, Type, Kind}
import com.gilt.apidoc.spec.v0.models.{Method, Model, Parameter, ParameterLocation, Operation, Resource}
import generator._
import org.scalatest.{ ShouldMatchers, FunSpec }

class GeneratorUtilSpec extends FunSpec with ShouldMatchers {

  private lazy val service = TestHelper.referenceApiService
  private lazy val ssd = new ScalaService(service)

  private val play2Util = GeneratorUtil(
    ScalaClientMethodConfigs.Play22("test.apidoc")
  )

  describe("params") {
    val model = new Model("model", "models", None, None, Nil)
    val q1 = new Parameter(
      "q1",
      "double",
      ParameterLocation.Query,
      None, None, true, None, None, None, None
    )
    val q2 = new Parameter(
      "q2",
      "double",
      ParameterLocation.Query,
      None, None, false, None, None, None, None
    )
    val operation = new Operation(Method.Get, "/models", None, None, None, Seq(q1, q2), Nil)
    val resource = new Resource(model.name, model.plural, None, None, Seq(operation))

    it("should handle required and non-required params") {
      val scalaModel = new ScalaModel(ssd, model)
      val code = play2Util.queryParameters(
        "queryParameters",
        new ScalaOperation(
          ssd,
          operation,
          new ScalaResource(ssd, resource)
        ).queryParameters
      )
      code.get should equal("""val queryParameters = Seq(
  Some("q1" -> q1.toString),
  q2.map("q2" -> _.toString)
).flatten""")
    }
  }

  it("supports query parameters that contain lists") {
    val operation = ssd.resources.find(_.plural == "Echoes").get.operations.head
    val code = play2Util.queryParameters("queryParameters", operation.queryParameters).get
    code should be("""
val queryParameters = Seq(
  foo.map("foo" -> _)
).flatten ++
  optionalMessages.getOrElse(Nil).map("optional_messages" -> _) ++
  requiredMessages.map("required_messages" -> _)
""".trim)
  }

  it("supports query parameters that ONLY have lists") {
    val operation = ssd.resources.find(_.plural == "Echoes").get.operations.find(_.path == "/echoes/arrays-only").get
    val code = play2Util.queryParameters("queryParameters", operation.queryParameters).get
    code should be("""
val queryParameters = optionalMessages.getOrElse(Nil).map("optional_messages" -> _) ++
  requiredMessages.map("required_messages" -> _)
""".trim)
  }

  it("supports optional seq query parameters") {
    val operation = ssd.resources.find(_.plural == "Users").get.operations.find(op => op.method == Method.Get && op.path == "/users").get

    TestHelper.assertEqualsFile(
      "test/resources/generators/play-2-route-util-reference-get-users.txt",
      play2Util.queryParameters("queryParameters", operation.queryParameters).get
    )
  }

}
