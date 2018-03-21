package models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.{FunSpec, Matchers}

import scala.models.http4s.Http4s018Generator

class CirceDefaultsSpec extends FunSpec with Matchers {
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
              "default": "2018-03-21T02:20:52Z",
              "attributes": []
          }],
          "attributes": [],
          "description": "Model of a car."
      }],
      "resources": []
      """)

    val form = InvocationForm(models.TestHelper.service(json.format()))


    val Right(res) = Http4s018Generator.invoke(form)

    val jsOnly = res.filter(_.name.contains("ModelsJson")).head

    jsOnly.contents.contains("""case class Model(
                                 |    guid: _root_.java.util.UUID = _root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd"),
                                 |    name: String = "M3",
                                 |    `type`: test.apidoc.apidoctest.v0.models.CarType = test.apidoc.apidoctest.v0.models.CarType.Coupe,
                                 |    curbWeight: Int = 3500,
                                 |    serial: Long = 45678901234L,
                                 |    finalDrive: Double = 3.85,
                                 |    msrp: BigDecimal = 45999.99,
                                 |    isFlashy: Boolean = true,
                                 |    markets: Seq[String] = scala.List("USA","CAN"),
                                 |    launchedOn: _root_.java.time.LocalDate = _root_.java.time.LocalDate.parse("1986-02-01"),
                                 |    timestamp: _root_.java.time.Instant = _root_.java.time.Instant.parse("2018-03-21T02:20:52Z")""".stripMargin) should be(true)

    jsOnly.contents.contains("""for {
                                 |        guid <- c.downField("guid").as[Option[_root_.java.util.UUID]]
                                 |        name <- c.downField("name").as[Option[String]]
                                 |        __type__ <- c.downField("type").as[Option[test.apidoc.apidoctest.v0.models.CarType]]
                                 |        curbWeight <- c.downField("curb_weight").as[Option[Int]]
                                 |        serial <- c.downField("serial").as[Option[Long]]
                                 |        finalDrive <- c.downField("final_drive").as[Option[Double]]
                                 |        msrp <- c.downField("msrp").as[Option[BigDecimal]]
                                 |        isFlashy <- c.downField("is_flashy").as[Option[Boolean]]
                                 |        markets <- c.downField("markets").as[Option[Seq[String]]]
                                 |        launchedOn <- c.downField("launched_on").as[Option[_root_.java.time.LocalDate]]
                                 |        timestamp <- c.downField("timestamp").as[Option[_root_.java.time.Instant]]
                                 |      } yield {
                                 |        val ret = Model(
                                 |
                                 |        )
                                 |        Some(ret)
                                 |          .map(m=> guid.map(d=> m.copy(guid=d)).getOrElse(m))
                                 |          .map(m=> name.map(d=> m.copy(name=d)).getOrElse(m))
                                 |          .map(m=> __type__.map(d=> m.copy(`type`=d)).getOrElse(m))
                                 |          .map(m=> curbWeight.map(d=> m.copy(curbWeight=d)).getOrElse(m))
                                 |          .map(m=> serial.map(d=> m.copy(serial=d)).getOrElse(m))
                                 |          .map(m=> finalDrive.map(d=> m.copy(finalDrive=d)).getOrElse(m))
                                 |          .map(m=> msrp.map(d=> m.copy(msrp=d)).getOrElse(m))
                                 |          .map(m=> isFlashy.map(d=> m.copy(isFlashy=d)).getOrElse(m))
                                 |          .map(m=> markets.map(d=> m.copy(markets=d)).getOrElse(m))
                                 |          .map(m=> launchedOn.map(d=> m.copy(launchedOn=d)).getOrElse(m))
                                 |          .map(m=> timestamp.map(d=> m.copy(timestamp=d)).getOrElse(m))
                                 |          .get
                                 |      }
                                 |    }""".stripMargin) should be(true)
  }
}
