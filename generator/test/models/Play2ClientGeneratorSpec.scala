package models

import com.gilt.apidoc.spec.v0.models.Method
import generator.{ScalaDatatype, ScalaPrimitive, ScalaService, ScalaClientMethodGenerator, ScalaClientMethodConfigs}
import org.scalatest.{ShouldMatchers, FunSpec}

class Play2ClientGeneratorSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play22("test.apidoc")

  it("errorTypeClass") {
    val service = TestHelper.generatorApiService
    val ssd = new ScalaService(service)
    val resource = ssd.resources.find(_.plural == "Invocations").getOrElse {
      sys.error("could not find resource with name[Invocations]")
    }
    val operation = resource.operations.find(_.method == Method.Post).get
    val errorResponse = operation.responses.find(_.code == 409).get
    errorResponse.errorClassName should be("ErrorsResponse")
    errorResponse.datatype.name should be("Seq[com.gilt.apidoc.generator.v0.models.Error]")

    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    TestHelper.assertEqualsFile("test/resources/generators/play2-client-generator-spec-errors-package.txt", contents)
  }

  it("only generates error wrappers for model classes (not primitives)") {
    val json = TestHelper.buildJson(s"""
      "imports": [],
      "headers": [],
      "models": [],
      "enums": [],
      "unions": [],

      "models": [
        {
          "name": "user",
          "plural": "user",
          "fields": [
            { "name": "id", "type": "long", "required": true }
          ]
        }

      ],

      "resources": [
        {
          "type": "user",
          "plural": "users",
          "path": "/users",
          "operations": [
            {
              "method": "GET",
              "path": "/:id",
              "parameters": [],
              "responses": [
                { "code": 200, "type": "user" },
                { "code": 409, "type": "unit" }
              ]
            }
          ]
        }
      ]
    """)

    val ssd = TestHelper.scalaService(json)
    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    TestHelper.assertEqualsFile("test/resources/generators/play2-client-generator-spec-errors-package-no-models.txt", contents)
  }

}
