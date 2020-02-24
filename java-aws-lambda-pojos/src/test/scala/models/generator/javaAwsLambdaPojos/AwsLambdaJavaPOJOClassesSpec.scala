package models.generator.javaAwsLambdaPojos

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{AnnotationExpr, MemberValuePair, NormalAnnotationExpr}
import io.apibuilder.generator.v0.models.InvocationForm
import models.TestHelper.assertJodaTimeNotPresent
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

class AwsLambdaJavaPOJOClassesSpec
  extends AnyFunSpec
    with Matchers {

  def memberValuePairsToSimpleMap(pairs: java.util.List[MemberValuePair]): Map[String,String] = {
    (for(i <- 0 until pairs.size()) yield (pairs.get(i).getNameAsString -> pairs.get(i).getValue.toString)).toMap
  }

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
              "attributes": [
                {
                 "name": "DynamoDBHashKey",
                 "value": {
                   "attributeName": "modelId"
                 }
               }
              ]
          }, {
              "name": "name",
              "type": "string",
              "required": true,
              "attributes": [
                {
                 "name": "pattern",
                 "value": {
                    "regexp": "[A-Z][a-z]+"
                 }
                },
                {
                  "name": "DynamoDBRangeKey",
                  "value": {
                    "attributeName": "name"
                  }
                }
              ]
          }, {
              "name": "type",
              "type": "car_type",
              "required": true,
              "attributes": []
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": true,
              "attributes": [
              {
                "name": "size",
                "value": {
                "min": 1,
                "max": 5000
              }
             }
            ]
          }],
          "attributes": [
             {
              "name": "DynamoDBTable",
              "value": {
                "tableName": "models"
             }
           }
          ],
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

    val result = JavaAwsLambdaPOJOClasses.invoke(InvocationForm(models.TestHelper.service(json.format())))

    result.isRight should be(true)
    val files = result.getOrElse(sys.error("got Left"))
    files.size should be(2)
    assertJodaTimeNotPresent(files)
    files(0).name should be("CarType.java")
    files(1).name should be("Model.java")

    val javaParser = new JavaParser()
    val modelCompiled = javaParser.parse(files(1).contents).getResult.get

    val javaAnnotations = modelCompiled.findAll(classOf[NormalAnnotationExpr])
    val annotations: Set[String] = (for(i <- 0 until javaAnnotations.size()) yield javaAnnotations.get(i).getNameAsString).toSet
    it("should label DynamoDBTable")(assert(annotations.contains("DynamoDBTable")))
    it("should label all classes with JsonIgnoreProperties")(assert(annotations.contains("JsonIgnoreProperties")))

    val fields = modelCompiled.findAll(classOf[FieldDeclaration]).asScala

    fields.map(field => {
      assert(field.isProtected)
      val javaAnnotations: NodeList[AnnotationExpr] = field.getAnnotations
      val annotations: Set[String] = (for(i <- 0 until javaAnnotations.size()) yield javaAnnotations.get(i).getNameAsString).toSet
      assert(annotations.contains("NotNull"))
      field.getVariables.get(0).getNameAsString match {
        case "curb_weight" => it("should annotate sized variables")(assert(annotations.contains("Size")))
        case "name" => it("should annotate regex patterns")(assert(annotations.contains("Pattern")))
        case _ =>
      }
    })

    val methods = modelCompiled.findAll(classOf[MethodDeclaration]).asScala

    methods.map(method => {
      val javaAnnotations: NodeList[AnnotationExpr] = method.getAnnotations
      val annotations: Set[String] = (for(i <- 0 until javaAnnotations.size()) yield javaAnnotations.get(i).getNameAsString).toSet

      method.getNameAsString match {
        case "getGuid" => it("should annotate DynamoDBHashKey")(assert(annotations.contains("DynamoDBHashKey")))
        case "getModel"=> it("should annotate DynamoDBRangeKey")(assert(annotations.contains("DynamoDBRangeKey")))
        case _ =>
      }
    })

    //walk the javaAnnotations and confirm that they have the right members
    for(i <- 0 until javaAnnotations.size()){
      val annotationExpr = javaAnnotations.get(i)
      val pairs = memberValuePairsToSimpleMap(annotationExpr.getPairs)
      annotationExpr.getNameAsString match {
        case "Size" => {
          it("should have the minimum size")(assert(pairs.get("min").get == "1"))
          it("should have the maximum size")(assert(pairs.get("max").get == "5000"))
        }
        case "Pattern" => {
          it("should have any regexes we added")(assert(pairs.get("regexp").get == "\"[A-Z][a-z]+\""))
        }
        case "DynamoDBTable" => {
          it("should have the correct table name")(assert(pairs.get("tableName").get == "\"models\""))
        }
        case "DynamoDBRangeKey" => {
          it("should have the correct range key annotation")(assert(pairs.get("attributeName").get == "\"name\""))
        }
        case "DynamoDBHashKey" => {
          it("should have the correct hash key annotation")(assert(pairs.get("attributeName").get == "\"modelId\""))
        }
        case _ =>
      }
    }

  }

}
