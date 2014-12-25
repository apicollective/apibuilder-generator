package models

import com.gilt.apidocgenerator.models.Method
import generator.{ScalaDatatype, ScalaPrimitive, ScalaService, ScalaClientMethodGenerator, ScalaClientMethodConfigs}
import org.scalatest.{ShouldMatchers, FunSpec}

class Play2ClientGeneratorSpec extends FunSpec with ShouldMatchers {

  val clientMethodConfig = new ScalaClientMethodConfigs.Play {
    override def responseClass = PlayFrameworkVersions.V2_2_x.config.responseClass
  }

  it("errorTypeClass") {
    val service = TestHelper.parseFile("../api/api.json")
    val ssd = new ScalaService(service)
    val resource = ssd.resources("organizations")
    val operation = resource.operations.find(_.method == Method.Post).get
    val errorResponse = operation.responses.find(_.code == 409).get
    errorResponse.errorClassName should be("ErrorsResponse")
    errorResponse.datatype should be(ScalaDatatype.List(Seq(ScalaPrimitive.Model("apidoc.models.Errors"))))

    val contents = ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage()
    TestHelper.assertEqualsFile("test/resources/generators/play2-client-generator-spec-errors-package.txt", contents)
  }

  it("only generates error wrappers for model classes (not primitives)") {
    val json = """
    {
      "base_url": "http://localhost:9000",
      "name": "Api Doc",
      "models": {
        "user": {
          "fields": [
            { "name": "id", "type": "long" }
          ]
        }
      },
      "resources": {
        "user": {
          "operations": [
            {
              "method": "GET",
              "path": "/:id",
              "responses": {
                "200": { "type": "user" },
                "409": { "type": "unit" }
              }
            }
          ]
        }
      }
    }

    """

    val ssd = TestHelper.scalaService(json)
    ScalaClientMethodGenerator(clientMethodConfig, ssd).errorPackage() should be("")
  }

}
