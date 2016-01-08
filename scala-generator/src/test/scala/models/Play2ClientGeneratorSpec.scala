package scala.models

import com.bryzek.apidoc.spec.v0.models.Method
import scala.generator.{ScalaDatatype, ScalaPrimitive, ScalaService, ScalaClientMethodGenerator, ScalaClientMethodConfigs}
import org.scalatest.{ShouldMatchers, FunSpec}

class Play2ClientGeneratorSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = ScalaClientMethodConfigs.Play24("test.apidoc")

  it("errorTypeClass") {
    val service = models.TestHelper.generatorApiService
    val ssd = new ScalaService(service)
    val resource = ssd.resources.find(_.plural == "Invocations").getOrElse {
      sys.error("could not find resource with name[Invocations]")
    }
    val operation = resource.operations.find(_.method == Method.Post).get
    val errorResponse = operation.responses.find(r => models.TestHelper.responseCode(r.code) == "409").get
    errorResponse.errorClassName should be("ErrorsResponse")
    errorResponse.datatype.name should be("Seq[com.bryzek.apidoc.generator.v0.models.Error]")

    val contents = new ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package.txt", contents)
  }

  it("only generates error wrappers for model classes (not primitives)") {
    val json = models.TestHelper.buildJson(s"""
      "imports": [],
      "headers": [],
      "info": [],
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
                { "code": { "integer": { "value": 200 } }, "type": "user" },
                { "code": { "integer": { "value": 409 } }, "type": "unit" }
              ]
            }
          ]
        }
      ]
    """)

    val ssd = ScalaService(models.TestHelper.service(json))
    val contents = new ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package-no-models.txt", contents)
  }

/*
  it("model, enum and union use case - https://github.com/mbryzek/apidoc/issues/384") {
    val json = models.TestHelper.readFile("lib/src/test/resources/generators/play-2-union-model-enum-service.json")
    val ssd = ScalaService(models.TestHelper.service(json))
    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    models.TestHelper.assertEqualsFile("/generators/play2-client-generator-spec-errors-package-no-models.txt", contents)
  }
 */
}
