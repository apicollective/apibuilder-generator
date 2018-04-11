package models

import go.models.GoClientGenerator
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source

class GoReceiverDefaultsSpec extends FunSpec with Matchers {
  it("Should produce models with defaults") {
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
              "required": false,
              "default" : "abcd-ef01-2345-6789-abcd",
              "attributes": []
          }, {
              "name": "name",
              "type": "string",
              "required": false,
              "default": "M3",
              "attributes": []
          }, {
              "name": "type",
              "type": "car_type",
              "required": false,
              "default": "coupe",
              "attributes": []
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": false,
              "default": "3500",
              "attributes": []
          },{
              "name": "serial",
              "type": "long",
              "required": false,
              "default": "45678901234",
              "attributes": []
          }, {
              "name": "final_drive",
              "type": "double",
              "required": false,
              "default": "3.85",
              "attributes": []
          }, {
              "name": "msrp",
              "type": "decimal",
              "required": false,
              "default": "45999.99",
              "attributes": []
          }, {
              "name": "is_flashy",
              "type": "boolean",
              "required": false,
              "default": "true",
              "attributes": []
          },{
              "name": "markets",
              "type": "[string]",
              "required": false,
              "default": "[\"USA\",\"CAN\"]",
              "attributes": []
          },{
              "name": "launched_on",
              "type": "date-iso8601",
              "required": false,
              "default": "1986-02-01",
              "attributes": []
          },{
              "name": "timestamp",
              "type": "date-time-iso8601",
              "required": false,
              "default": "2018-03-21T02:20:52+00:00",
              "attributes": []
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = GoClientGenerator.invoke(form)

    res.head.contents.contains("""func ModelFromJson(bytes io.Reader) Model {
                   |	var model Model
                   |	model.Guid = "abcd-ef01-2345-6789-abcd"
                   |	model.Name = "M3"
                   |	model.Type = CarTypeCoupe
                   |	model.CurbWeight = 3500
                   |	model.Serial = 45678901234
                   |	model.FinalDrive = 3.85
                   |	model.Msrp = 45999.99
                   |	model.IsFlashy = "true"
                   |	model.Markets = []string{"USA","CAN"}
                   |	model.LaunchedOn = "1986-02-01"
                   |	model.Timestamp = "2018-03-21T02:20:52+00:00"
                   |	json.NewDecoder(bytes).Decode(&model)
                   |	return model
                   |}""".stripMargin) should be(true)
  }
}
