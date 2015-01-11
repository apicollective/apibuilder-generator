package generator

import models.TestHelper
import lib.Primitives
import com.gilt.apidoc.spec.models._
import org.scalatest._

class ScalaOperationSpec extends FunSpec with ShouldMatchers {

  private lazy val service = TestHelper.referenceApiService
  private lazy val ssd = new ScalaService(service)

  val q1 = new Parameter(
    "q1",
    "double",
    ParameterLocation.Query,
    None, false, None, None, None, None)

  it("models as a parameter in the body should use capitalize") {
    val body = Body("user")
    val model = new Model("user", "users", None, Nil)
    val operation = new Operation(Method.Get, "/users", None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model, None, Seq(operation))
    val scalaModel = new ScalaModel(ssd, model)

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource, scalaModel)
    )

    scalaOperation.argList shouldEqual Some("user: com.gilt.apidoc.reference.api.models.User,\n  q1: _root_.scala.Option[Double] = None\n")
  }

  it("array of models as a parameter in the body should pluralize model name") {
    val body = Body("[user]", None)
    val model = new Model("user", "users", None, Nil)
    val operation = new Operation(Method.Get, "/users", None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model, None, Seq(operation))
    val scalaModel = new ScalaModel(ssd, model)

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource, scalaModel)
    )

    scalaOperation.argList shouldEqual Some("users: Seq[com.gilt.apidoc.reference.api.models.User],\n  q1: _root_.scala.Option[Double] = None\n")
  }

  it("primitive type as a parameter in the body should not use capitalize") {
    val body = Body("integer", None)
    val model = new Model("model", "models", None, Nil)
    val operation = new Operation(Method.Get, "/models", None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model, None, Seq(operation))
    val scalaModel = new ScalaModel(ssd, model)

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource, scalaModel)
    )

    scalaOperation.argList shouldEqual Some("value: Int,\n  q1: _root_.scala.Option[Double] = None\n")
  }

  it("array of primitive types as a parameter in the body should not use capitalize") {
    val body = Body("[integer]", None)
    val model = new Model("model", "models", None, Nil)
    val operation = new Operation(Method.Get, "/models", None, Some(body), Seq(q1), Nil)
    val resource = new Resource(model, None, Seq(operation))
    val scalaModel = new ScalaModel(ssd, model)

    val scalaOperation = new ScalaOperation(
      ssd,
      operation,
      new ScalaResource(ssd, resource, scalaModel)
    )

    scalaOperation.argList shouldEqual Some("values: Seq[Int],\n  q1: _root_.scala.Option[Double] = None\n")

  }

}
