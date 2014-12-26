package generator

import models.TestHelper
import lib.Primitives
import com.gilt.apidocgenerator.models._
import org.scalatest._

class ScalaOperationSpec extends FunSpec with ShouldMatchers {

  private lazy val service = TestHelper.referenceApiService
  private lazy val ssd = new ScalaService(service)

  val q1 = new Parameter(
    "q1",
    "double",
    Some(ParameterLocation.Query),
    None, Some(false), None, None, None, None)

  it("models as a parameter in the body should use capitalize") {
    val body = com.gilt.apidocgenerator.models.Body("model")
    val model = new Model(Some("models"), None, Nil)
    val operation = new Operation(Method.Get, Some("models"), None, Some(body), Seq(q1), Map.empty)
    val resource = new Resource(None, Some("models"), Seq(operation))
    val scalaModel = new ScalaModel(ssd, "model", model)

    val scalaOperation = new ScalaOperation(
      ssd,
      scalaModel,
      operation,
      new ScalaResource(ssd, scalaModel, resource)
    )

    scalaOperation.argList shouldEqual Some("model: apidocreferenceapi.models.Model,\n  q1: _root_.scala.Option[Double] = None\n")
  }

  it("array of models as a parameter in the body should use capitalize") {
    val body = com.gilt.apidocgenerator.models.Body("model", None)
    val model = new Model(Some("models"), None, Nil)
    val operation = new Operation(Method.Get, Some("models"), None, Some(body), Seq(q1), Map.empty)
    val resource = new Resource(None, Some("models"), Seq(operation))
    val scalaModel = new ScalaModel(ssd, "model", model)

    val scalaOperation = new ScalaOperation(
      ssd,
      scalaModel,
      operation,
      new ScalaResource(ssd, scalaModel, resource)
    )

    scalaOperation.argList shouldEqual Some("models: Seq[apidocreferenceapi.models.Model],\n  q1: _root_.scala.Option[Double] = None\n")
  }

  it("primitive type as a parameter in the body should not use capitalize") {
    val body = com.gilt.apidocgenerator.models.Body("integer", None)
    val model = new Model(Some("models"), None, Nil)
    val operation = new Operation(Method.Get, Some("models"), None, Some(body), Seq(q1), Map.empty)
    val resource = new Resource(None, Some("models"), Seq(operation))
    val scalaModel = new ScalaModel(ssd, "model", model)

    val scalaOperation = new ScalaOperation(
      ssd,
      scalaModel,
      operation,
      new ScalaResource(ssd, scalaModel, resource)
    )

    scalaOperation.argList shouldEqual Some("value: Int,\n  q1: _root_.scala.Option[Double] = None\n")
  }

  it("array of primitive types as a parameter in the body should not use capitalize") {
    val body = com.gilt.apidocgenerator.models.Body("[integer]", None)
    val model = new Model(Some("models"), None, Nil)
    val operation = new Operation(Method.Get, Some("models"), None, Some(body), Seq(q1), Map.empty)
    val resource = new Resource(None, Some("models"), Seq(operation))
    val scalaModel = new ScalaModel(ssd, "model", model)

    val scalaOperation = new ScalaOperation(
      ssd,
      scalaModel,
      operation,
      new ScalaResource(ssd, scalaModel, resource)
    )

    scalaOperation.argList shouldEqual Some("values: Seq[Int],\n  q1: _root_.scala.Option[Double] = None\n")

  }

}
