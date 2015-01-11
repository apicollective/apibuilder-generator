package models

import lib.{Datatype, Primitives, Type, TypeKind}
import com.gilt.apidocspec.models.{Method, Model, Parameter, ParameterLocation, Operation, Resource}
import generator._
import org.scalatest.{ ShouldMatchers, FunSpec }

class GeneratorUtilSpec extends FunSpec with ShouldMatchers {

  private lazy val service = TestHelper.referenceApiService
  private lazy val ssd = new ScalaService(service)

  private val play2Util = GeneratorUtil(new ScalaClientMethodConfigs.Play {
    override def responseClass = PlayFrameworkVersions.V2_2_x.config.responseClass
  })

  describe("params") {
    val model = new Model("model", "models", None, Nil)
    val q1 = new Parameter(
      "q1",
      "double",
      ParameterLocation.Query,
      None, true, None, None, None, None
    )
    val q2 = new Parameter(
      "q2",
      "double",
      ParameterLocation.Query,
      None, false, None, None, None, None
    )
    val operation = new Operation(Method.Get, "/models", None, None, Seq(q1, q2), Nil)
    val resource = new Resource(model, None, Seq(operation))

    it("should handle required and non-required params") {
      val scalaModel = new ScalaModel(ssd, model)
      val code = play2Util.params(
        "queryParameters",
        new ScalaOperation(
          ssd,
          operation,
          new ScalaResource(ssd, resource, scalaModel)
        ).queryParameters
      )
      code.get should equal("""val queryParameters = Seq(
  Some("q1" -> q1.toString),
  q2.map("q2" -> _.toString)
).flatten""")
    }
  }

  it("supports query parameters that contain lists") {
    val operation = ssd.resources.find(_.model.name == "Echo").get.operations.head
    val code = play2Util.params("queryParameters", operation.queryParameters).get
    code should be("""
val queryParameters = Seq(
  foo.map("foo" -> _)
).flatten ++
  optionalMessages.map("optional_messages" -> _) ++
  requiredMessages.map("required_messages" -> _)
""".trim)
  }

  it("supports query parameters that ONLY have lists") {
    val operation = ssd.resources.find(_.model.name == "Echo").get.operations.find(_.path == "/echoes/arrays-only").get
    val code = play2Util.params("queryParameters", operation.queryParameters).get
    code should be("""
val queryParameters = optionalMessages.map("optional_messages" -> _) ++
  requiredMessages.map("required_messages" -> _)
""".trim)
  }

  it("supports optional seq query parameters") {
    val operation = ssd.resources.find(_.model.name == "User").get.operations.find(op => op.method == Method.Get && op.path == "/users").get

    TestHelper.assertEqualsFile(
      "test/resources/generators/play-2-route-util-reference-get-users.txt",
      play2Util.params("queryParameters", operation.queryParameters).get
    )
  }

}
