package scala.models

import scala.generator.{ScalaCaseClasses, ScalaClientMethodConfigs, ScalaService}
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScalaNestedUnionSpec extends AnyFunSpec with Matchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play23("test.apidoc", Attributes.PlayDefaultConfig, None)

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
        },
        {
          "name": "second_outer_type",
          "plural": "second_outer_types",
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

  it("generates valid nested union traits") {
    ScalaCaseClasses.invoke(InvocationForm(service), addHeader = false) match {
      case Left(errors) => fail(errors.mkString(", "))
      case Right(sourceFiles) => {
        sourceFiles.size shouldBe 1
        models.TestHelper.assertEqualsFile("/scala-nested-union-models-case-classes.txt", sourceFiles.head.contents)
      }
    }
  }
}
