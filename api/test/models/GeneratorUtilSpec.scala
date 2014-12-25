package models

import lib.{Datatype, Primitives, Type, TypeKind}
import com.gilt.apidocgenerator.models.{Method, Model, Parameter, ParameterLocation, Operation, Resource}
import generator._
import org.scalatest.{ ShouldMatchers, FunSpec }

class GeneratorUtilSpec extends FunSpec with ShouldMatchers {

  private lazy val service = TestHelper.parseFile("reference-api/api.json")
  private lazy val ssd = new ScalaService(service)

  private val play2Util = GeneratorUtil(new ScalaClientMethodConfigs.Play {
    override def responseClass = PlayFrameworkVersions.V2_2_x.config.responseClass
  })

  describe("params") {
    val model = new Model(Some("models"), None, Nil)
    val q1 = new Parameter(
      "q1",
      "doube",
      Some(ParameterLocation.Query),
      None, Some(true), None, None, None, None
    )
    val q2 = new Parameter(
      "q2",
      "doube",
      Some(ParameterLocation.Query),
      None, Some(false), None, None, None, None
    )
    val operation = new Operation(Method.Get, Some("models"), None, None, Seq(q1, q2), Map.empty)
    val resource = new Resource(None, Some("models"), Seq(operation))

    it("should handle required and non-required params") {
      val scalaModel = new ScalaModel(ssd, "model", model)
      val code = play2Util.params(
        "queryParameters",
        new ScalaOperation(
          ssd,
          scalaModel,
          operation,
          new ScalaResource(ssd, scalaModel, resource)
        ).queryParameters
      )
      code.get should equal("""val queryParameters = Seq(
  Some("q1" -> q1.toString),
  q2.map("q2" -> _.toString)
).flatten""")
    }
  }

  it("supports query parameters that contain lists") {
    val operation = ssd.resources("echoes").operations.head
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
    val operation = ssd.resources("echoes").operations.find(_.path == Some("/echoes/arrays-only")).get
    val code = play2Util.params("queryParameters", operation.queryParameters).get
    code should be("""
val queryParameters = optionalMessages.map("optional_messages" -> _) ++
  requiredMessages.map("required_messages" -> _)
""".trim)
  }

  describe("with reference-api service") {
    lazy val service = TestHelper.parseFile(s"reference-api/api.json")

    it("supports optional seq  query parameters") {
      val operation = ssd.resources("users").operations.find(op => op.method == Method.Get && op.path == Some("/users")).get

      TestHelper.assertEqualsFile(
        "test/resources/generators/play-2-route-util-reference-get-users.txt",
        play2Util.params("queryParameters", operation.queryParameters).get
      )
    }

  }

}
