package models.generator.android

import io.apibuilder.generator.v0.models.InvocationForm
import com.github.javaparser.JavaParser
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class TestAndroidClasses
  extends AnyFunSpec
    with Matchers {

  describe("for a model with 2 enum fields") {

    val json = models.TestHelper.buildJson(
      """
      "imports": [],
      "headers": [],
      "info": [],
      "unions": [],
      "resources": [],
      "attributes": [],
      "enums": [{
          "name": "car_type",
          "plural": "car_types",
          "values": [{
              "name": "sedan",
              "attributes": [],
              "description": "most common"
          }, {
              "name": "coupe",
              "attributes": []
          }, {
              "name": "cabriolet",
              "attributes": [],
              "description": "the coolest"
          }, {
              "name": "wagon",
              "attributes": []
          }],
          "attributes": [],
          "description": "on has originated"
      }],
      "models": [{
          "name": "model",
          "plural": "models",
          "fields": [{
              "name": "guid",
              "type": "uuid",
              "required": true,
              "attributes": []
          }, {
              "name": "name",
              "type": "string",
              "required": true,
              "attributes": []
          }, {
              "name": "type",
              "type": "car_type",
              "required": true,
              "attributes": []
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": true,
              "attributes": []
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": [{
        "type": "model",
        "plural": "models",
        "operations": [{
            "method": "GET",
              "path": "/model",
              "parameters": [],
              "responses": [{
                  "code": {
                      "integer": {
                          "value": 200
                      }
                  },
                  "type": "[model]",
                  "headers": []
              }, {
                  "code": {
                      "integer": {
                          "value": 404
                      }
                  },
                  "type": "unit",
                  "headers": []
              }],
              "attributes": [],
              "description": "get all models"
          }, {
              "method": "GET",
              "path": "/model/:guid",
              "parameters": [{
                  "name": "guid",
                  "type": "uuid",
                  "location": "Path",
                  "required": true
              }],
              "responses": [{
                  "code": {
                      "integer": {
                          "value": 200
                       }
                  },
                  "type": "model",
                  "headers": []
              }, {
                  "code": {
                      "integer": {
                          "value": 404
                      }
                  },
                  "type": "unit",
                  "headers": []
              }],
              "attributes": []
          }],
          "attributes": [],
          "path": "/model"
      }]
      """)

    val result = AndroidClasses.invoke(InvocationForm(models.TestHelper.service(json.format())))

    result.isRight should be(true)
    val files = result.getOrElse(sys.error("got Left"))
    files.size should be(4)
    files(0).name should be("CarType.java")
    files(1).name should be("Model.java")
    files(2).name should be("ApiBuilderObjectMapper.java")
    files(3).name should be("ModelsClient.java")
    val javaParser = new JavaParser()
    files.foreach { file =>
      javaParser.parse(file.contents).isSuccessful shouldBe true
    }
  }

}
