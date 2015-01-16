package models

import com.gilt.apidoc.spec.models.Method
import generator.{ScalaDatatype, ScalaPrimitive, ScalaService, ScalaClientMethodGenerator, ScalaClientMethodConfigs}
import org.scalatest.{ShouldMatchers, FunSpec}

class Play2ClientGeneratorSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = new ScalaClientMethodConfigs.Play {
    override def responseClass = PlayFrameworkVersions.V2_2_x.config.responseClass
  }

  it("errorTypeClass") {
    val service = TestHelper.generatorApiService
    val ssd = new ScalaService(service)
    val resource = ssd.resources.find(_.model.name == "Invocation").get
    val operation = resource.operations.find(_.method == Method.Post).get
    val errorResponse = operation.responses.find(_.code == 409).get
    errorResponse.errorClassName should be("ErrorsResponse")
    errorResponse.datatype.name should be("Seq[com.gilt.apidoc.generator.v0.models.Error]")

    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    TestHelper.assertEqualsFile("test/resources/generators/play2-client-generator-spec-errors-package.txt", contents)
  }

  it("only generates error wrappers for model classes (not primitives)") {
    val userModel = """
        {
          "name": "user",
          "plural": "user",
          "fields": [
            { "name": "id", "type": "long", "required": true }
          ]
        }
    """.trim

    val json = TestHelper.buildJson(s"""
      "models": [$userModel],
      "resources": [
        {
          "model": $userModel,
          "path": "/users",
          "operations": [
            {
              "method": "GET",
              "path": "/:id",
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
    ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage() should be("")
  }

}
