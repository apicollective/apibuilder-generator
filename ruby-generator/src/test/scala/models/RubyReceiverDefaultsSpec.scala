package models

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, Matchers}
import ruby.models.RubyClientGenerator

class RubyReceiverDefaultsSpec extends FunSpec with Matchers {
  it("should produce models with defaults") {
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
              "default" : "abcdef01-2345-6789-abcd-ef0123456789",
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

    val Right(res) = RubyClientGenerator.invoke(form)

    res.head.contents.contains("""            def initialize(incoming={})
                                 |              opts = HttpClient::Helper.symbolize_keys(incoming)
                                 |              @guid = HttpClient::Preconditions.assert_class('guid', HttpClient::Helper.to_uuid((x = opts.delete(:guid); x.nil? ? UUID.new("abcdef01-2345-6789-abcd-ef0123456789") : x)), String)
                                 |              @name = HttpClient::Preconditions.assert_class('name', (x = opts.delete(:name); x.nil? ? "M3" : x), String)
                                 |              @type = (x = (x = opts.delete(:type); x.nil? ? "coupe" : x); x.is_a?(::Test::Apidoc::Apidoctest::V0::Models::CarType) ? x : ::Test::Apidoc::Apidoctest::V0::Models::CarType.apply(x))
                                 |              @curb_weight = HttpClient::Preconditions.assert_class('curb_weight', (x = opts.delete(:curb_weight); x.nil? ? 3500 : x), Integer)
                                 |              @serial = HttpClient::Preconditions.assert_class('serial', (x = opts.delete(:serial); x.nil? ? 45678901234 : x), Integer)
                                 |              @final_drive = HttpClient::Preconditions.assert_class('final_drive', (x = opts.delete(:final_drive); x.nil? ? 3.85 : x), Numeric)
                                 |              @msrp = HttpClient::Preconditions.assert_class('msrp', HttpClient::Helper.to_big_decimal((x = opts.delete(:msrp); x.nil? ? 45999.99 : x)), BigDecimal)
                                 |              @is_flashy = HttpClient::Preconditions.assert_boolean('is_flashy', (x = opts.delete(:is_flashy); x.nil? ? true : x))
                                 |              @markets = HttpClient::Preconditions.assert_class('markets', (x = opts.delete(:markets); x.nil? ? ["USA","CAN"] : x), Array).map { |v| HttpClient::Preconditions.assert_class('markets', v, String) }
                                 |              @launched_on = HttpClient::Preconditions.assert_class('launched_on', HttpClient::Helper.to_date_iso8601((x = opts.delete(:launched_on); x.nil? ? Date.parse("1986-02-01") : x)), Date)
                                 |              @timestamp = HttpClient::Preconditions.assert_class('timestamp', HttpClient::Helper.to_date_time_iso8601((x = opts.delete(:timestamp); x.nil? ? DateTime.parse("2018-03-21T02:20:52+00:00") : x)), DateTime)
                                 |            end""".stripMargin) should be(true)
  }
}
