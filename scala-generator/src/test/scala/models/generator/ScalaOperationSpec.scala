package scala.generator

import io.apibuilder.spec.v0.models.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScalaOperationSpec extends AnyFunSpec with Matchers {

  private lazy val service = models.TestHelper.referenceApiService
  private lazy val ssd = new ScalaService(service)

  val q1 = new Parameter(
    "q1",
    "double",
    ParameterLocation.Query,
    None, None, false, None, None, None, None)

  it("models as a parameter in the body should use capitalize") {
    val body = Body("user")
    val model = new Model("user", "users", None, None, Nil)
    val operation = new Operation(Method.Get, "/users", None, None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model.name, model.plural, Some("/users"), None, None, Seq(operation))

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource)
    )

    scalaOperation.argList().map(_.trim) shouldEqual Some("user: io.apibuilder.reference.api.v0.models.User,\n  q1: _root_.scala.Option[Double] = None")

    // Test extra args
    scalaOperation.argList(Seq("value: Option[String] = None")).map(_.trim) shouldEqual Some("user: io.apibuilder.reference.api.v0.models.User,\n  q1: _root_.scala.Option[Double] = None,\n  value: Option[String] = None")
  }

  it("array of models as a parameter in the body should pluralize model name") {
    val body = Body("[user]", None)
    val model = new Model("user", "users", None, None, Nil)
    val operation = new Operation(Method.Get, "/users", None, None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model.name, model.plural, Some("/users"), None, None, Seq(operation))

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource)
    )

    scalaOperation.argList().map(_.trim) shouldEqual Some("users: Seq[io.apibuilder.reference.api.v0.models.User],\n  q1: _root_.scala.Option[Double] = None")
  }

  it("primitive type as a parameter in the body should not use capitalize") {
    val body = Body("integer", None)
    val model = new Model("model", "models", None, None, Nil)
    val operation = new Operation(Method.Get, "/models", None, None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model.name, model.plural, Some("/models"), None, None, Seq(operation))

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource)
    )

    scalaOperation.argList().map(_.trim) shouldEqual Some("value: Int,\n  q1: _root_.scala.Option[Double] = None")
  }

  it("array of primitive types as a parameter in the body should not use capitalize") {
    val body = Body("[integer]", None)
    val model = new Model("model", "models", None, None, Nil)
    val operation = new Operation(Method.Get, "/models", None, None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model.name, model.plural, Some("/models"), None, None, Seq(operation))

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource)
    )

    scalaOperation.argList().map(_.trim) shouldEqual Some("values: Seq[Int],\n  q1: _root_.scala.Option[Double] = None")
  }

}
