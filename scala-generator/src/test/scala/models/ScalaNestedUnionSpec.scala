package scala.models

import scala.generator.{ScalaCaseClasses, ScalaClientMethodConfigs, ScalaService}
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{Matchers, FunSpec}

class ScalaNestedUnionSpec extends FunSpec with Matchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play23("test.apidoc", None)

  val json = models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "enums": [],
      "resources": [],
      "attributes": [],
      "unions": [
        {
          "name": "inner_type",
          "plural": "inner_types",
          "attributes": [],
          "types": [
            {
              "type": "string_model",
              "attributes": []
            }
          ]
        },
        {
          "name": "outer_type",
          "plural": "outer_types",
          "attributes": [],
          "types": [
            {
              "type": "inner_type",
              "attributes": []
            }
          ]
        }
      ],
      "models": [
        {
          "name": "string_model",
          "plural": "string_models",
          "attributes": [],
          "fields": [
            {
              "name": "name",
              "type": "string",
              "required": true,
              "attributes": []
            }
          ]
        }
      ]
  """)

  lazy val service = models.TestHelper.service(json)
  lazy val ssd = ScalaService(service)

  it("generates valid inner type readers") {
    val innerType = ssd.unions.find(_.name == "InnerType").get
    val code = Play2Json(ssd).readers(innerType)
    models.TestHelper.assertEqualsFile("/scala-nested-union-models-json-union-type-readers-inner-type.txt", code)
  }

  it("generates valid outer type readers") {
    val outerType = ssd.unions.find(_.name == "OuterType").get
    val code = Play2Json(ssd).readers(outerType)
    models.TestHelper.assertEqualsFile("/scala-nested-union-models-json-union-type-readers-outer-type.txt", code)
  }
}
