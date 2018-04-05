package models

object FieldDefaultHelper {
  def requiredTrueDefaultNull = {
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
              "required": true,
              "default": null,
              "attributes": []
          }, {
              "name": "name",
              "type": "string",
              "required": true,
              "default": null,
              "attributes": []
          }, {
              "name": "type",
              "type": "car_type",
              "required": true,
              "default": null,
              "attributes": []
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": true,
              "default": null,
              "attributes": []
          },{
              "name": "serial",
              "type": "long",
              "required": true,
              "default": null,
              "attributes": []
          }, {
              "name": "final_drive",
              "type": "double",
              "required": true,
              "default": null,
              "attributes": []
          }, {
              "name": "msrp",
              "type": "decimal",
              "required": true,
              "default": null,
              "attributes": []
          }, {
              "name": "is_flashy",
              "type": "boolean",
              "required": true,
              "default": null,
              "attributes": []
          },{
              "name": "markets",
              "type": "[string]",
              "required": true,
              "default": null,
              "attributes": []
          },{
              "name": "launched_on",
              "type": "date-iso8601",
              "required": true,
              "default": null,
              "attributes": []
          },{
              "name": "timestamp",
              "type": "date-time-iso8601",
              "required": true,
              "default": null,
              "attributes": []
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """
  }

  def requiredFalseDefaultNull = {
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
              "default": null,
              "attributes": []
          }, {
              "name": "name",
              "type": "string",
              "required": false,
              "default": null,
              "attributes": []
          }, {
              "name": "type",
              "type": "car_type",
              "required": false,
              "default": null,
              "attributes": []
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": false,
              "default": null,
              "attributes": []
          },{
              "name": "serial",
              "type": "long",
              "required": false,
              "default": null,
              "attributes": []
          }, {
              "name": "final_drive",
              "type": "double",
              "required": false,
              "default": null,
              "attributes": []
          }, {
              "name": "msrp",
              "type": "decimal",
              "required": false,
              "default": null,
              "attributes": []
          }, {
              "name": "is_flashy",
              "type": "boolean",
              "required": false,
              "default": null,
              "attributes": []
          },{
              "name": "markets",
              "type": "[string]",
              "required": false,
              "default": null,
              "attributes": []
          },{
              "name": "launched_on",
              "type": "date-iso8601",
              "required": false,
              "default": null,
              "attributes": []
          },{
              "name": "timestamp",
              "type": "date-time-iso8601",
              "required": false,
              "default": null,
              "attributes": []
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """
  }

  def requiredFalseWithDefaultsHintWireFriendly = {
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
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }, {
              "name": "name",
              "type": "string",
              "required": false,
              "default": "M3",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }, {
              "name": "type",
              "type": "car_type",
              "required": false,
              "default": "coupe",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": false,
              "default": "3500",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          },{
              "name": "serial",
              "type": "long",
              "required": false,
              "default": "45678901234",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }, {
              "name": "final_drive",
              "type": "double",
              "required": false,
              "default": "3.85",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }, {
              "name": "msrp",
              "type": "decimal",
              "required": false,
              "default": "45999.99",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }, {
              "name": "is_flashy",
              "type": "boolean",
              "required": false,
              "default": "true",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          },{
              "name": "markets",
              "type": "[string]",
              "required": false,
              "default": "[\"USA\",\"CAN\"]",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          },{
              "name": "launched_on",
              "type": "date-iso8601",
              "required": false,
              "default": "1986-02-01",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          },{
              "name": "timestamp",
              "type": "date-time-iso8601",
              "required": false,
              "default": "2018-03-21T02:20:52Z",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "wire_friendly"}}]
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """
  }


  def requiredFalseWithDefaultsHintDeveloperFriendly = {
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
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }, {
              "name": "name",
              "type": "string",
              "required": false,
              "default": "M3",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }, {
              "name": "type",
              "type": "car_type",
              "required": false,
              "default": "coupe",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }, {
              "name": "curb_weight",
              "type": "integer",
              "required": false,
              "default": "3500",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          },{
              "name": "serial",
              "type": "long",
              "required": false,
              "default": "45678901234",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }, {
              "name": "final_drive",
              "type": "double",
              "required": false,
              "default": "3.85",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }, {
              "name": "msrp",
              "type": "decimal",
              "required": false,
              "default": "45999.99",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }, {
              "name": "is_flashy",
              "type": "boolean",
              "required": false,
              "default": "true",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          },{
              "name": "markets",
              "type": "[string]",
              "required": false,
              "default": "[\"USA\",\"CAN\"]",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          },{
              "name": "launched_on",
              "type": "date-iso8601",
              "required": false,
              "default": "1986-02-01",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          },{
              "name": "timestamp",
              "type": "date-time-iso8601",
              "required": false,
              "default": "2018-03-21T02:20:52Z",
              "attributes": [{"name": "scala_generator", "value": {"model_hint": "developer_friendly"}}]
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """
  }

  def requiredFalseWithDefaultsNoHints = {
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
              "default": "2018-03-21T02:20:52Z",
              "attributes": []
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """
  }
}
