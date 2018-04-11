package models.http4s

import io.apibuilder.generator.v0.models.InvocationForm
import models.FieldDefaultHelper._
import org.scalatest.{FunSpec, Matchers}

import scala.models.http4s.Http4s018Generator

class CirceDefaultsSpec extends FunSpec with Matchers {
  it("Should produce models with defaults") {
    val json = models.TestHelper.buildJson(requiredFalseWithDefaultsNoHints)

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

    jsOnly.contents.contains("""    implicit def decodeApiDocTestModel: Decoder[Model] = Decoder.instance { c =>
                               |     for {
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
                               |        Model(
                               |          guid = guid.getOrElse(_root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd")),
                               |          name = name.getOrElse("M3"),
                               |          __type__ = `type`.getOrElse(test.apidoc.apidoctest.v0.models.CarType.Coupe),
                               |          curbWeight = curbWeight.getOrElse(3500),
                               |          serial = serial.getOrElse(45678901234L),
                               |          finalDrive = finalDrive.getOrElse(3.85),
                               |          msrp = msrp.getOrElse(45999.99),
                               |          isFlashy = isFlashy.getOrElse(true),
                               |          markets = markets.getOrElse(scala.List("USA","CAN")),
                               |          launchedOn = launchedOn.getOrElse(_root_.java.time.LocalDate.parse("1986-02-01")),
                               |          timestamp = timestamp.getOrElse(_root_.java.time.Instant.parse("2018-03-21T02:20:52Z"))
                               |        )
                               |      }
                               |    }""".stripMargin) should be(true)
  }


  it("Should produce models with defaults and scala_generator.model_hint == required") {
    val json = models.TestHelper.buildJson(requiredFalseWithDefaultsHintDeveloperFriendly)

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
                               |        Model(
                               |          guid = guid.getOrElse(_root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd")),
                               |          name = name.getOrElse("M3"),
                               |          __type__ = `type`.getOrElse(test.apidoc.apidoctest.v0.models.CarType.Coupe),
                               |          curbWeight = curbWeight.getOrElse(3500),
                               |          serial = serial.getOrElse(45678901234L),
                               |          finalDrive = finalDrive.getOrElse(3.85),
                               |          msrp = msrp.getOrElse(45999.99),
                               |          isFlashy = isFlashy.getOrElse(true),
                               |          markets = markets.getOrElse(scala.List("USA","CAN")),
                               |          launchedOn = launchedOn.getOrElse(_root_.java.time.LocalDate.parse("1986-02-01")),
                               |          timestamp = timestamp.getOrElse(_root_.java.time.Instant.parse("2018-03-21T02:20:52Z"))
                               |        )
                               |      }
                               |    }""".stripMargin) should be(true)
  }

  it("Should produce models with defaults and scala_generator . model_hint == optional") {
    val json = models.TestHelper.buildJson(requiredFalseWithDefaultsHintWireFriendly)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Http4s018Generator.invoke(form)

    val jsOnly = res.filter(_.name.contains("ModelsJson")).head

    jsOnly.contents.contains("""case class Model(
                               |    guid: _root_.scala.Option[_root_.java.util.UUID] = Some(_root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd")),
                               |    name: _root_.scala.Option[String] = Some("M3"),
                               |    `type`: _root_.scala.Option[test.apidoc.apidoctest.v0.models.CarType] = Some(test.apidoc.apidoctest.v0.models.CarType.Coupe),
                               |    curbWeight: _root_.scala.Option[Int] = Some(3500),
                               |    serial: _root_.scala.Option[Long] = Some(45678901234L),
                               |    finalDrive: _root_.scala.Option[Double] = Some(3.85),
                               |    msrp: _root_.scala.Option[BigDecimal] = Some(45999.99),
                               |    isFlashy: _root_.scala.Option[Boolean] = Some(true),
                               |    markets: _root_.scala.Option[Seq[String]] = Some(scala.List("USA","CAN")),
                               |    launchedOn: _root_.scala.Option[_root_.java.time.LocalDate] = Some(_root_.java.time.LocalDate.parse("1986-02-01")),
                               |    timestamp: _root_.scala.Option[_root_.java.time.Instant] = Some(_root_.java.time.Instant.parse("2018-03-21T02:20:52Z"))""".stripMargin) should be(true)

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
                               |        Model(
                               |          guid = guid.getOrElse(Some(_root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd"))),
                               |          name = name.getOrElse(Some("M3")),
                               |          __type__ = `type`.getOrElse(Some(test.apidoc.apidoctest.v0.models.CarType.Coupe)),
                               |          curbWeight = curbWeight.getOrElse(Some(3500)),
                               |          serial = serial.getOrElse(Some(45678901234L)),
                               |          finalDrive = finalDrive.getOrElse(Some(3.85)),
                               |          msrp = msrp.getOrElse(Some(45999.99)),
                               |          isFlashy = isFlashy.getOrElse(Some(true)),
                               |          markets = markets.getOrElse(Some(scala.List("USA","CAN"))),
                               |          launchedOn = launchedOn.getOrElse(Some(_root_.java.time.LocalDate.parse("1986-02-01"))),
                               |          timestamp = timestamp.getOrElse(Some(_root_.java.time.Instant.parse("2018-03-21T02:20:52Z")))
                               |        )
                               |      }""".stripMargin) should be(true)
  }

  it("Should produce models with no defaults and all fields optional when required = false , default = null") {
    val json = models.TestHelper.buildJson(requiredFalseDefaultNull)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Http4s018Generator.invoke(form)

    val jsOnly = res.filter(_.name.contains("ModelsJson")).head

    jsOnly.contents.contains("""case class Model(
                               |    guid: _root_.scala.Option[_root_.java.util.UUID] = None,
                               |    name: _root_.scala.Option[String] = None,
                               |    `type`: _root_.scala.Option[test.apidoc.apidoctest.v0.models.CarType] = None,
                               |    curbWeight: _root_.scala.Option[Int] = None,
                               |    serial: _root_.scala.Option[Long] = None,
                               |    finalDrive: _root_.scala.Option[Double] = None,
                               |    msrp: _root_.scala.Option[BigDecimal] = None,
                               |    isFlashy: _root_.scala.Option[Boolean] = None,
                               |    markets: _root_.scala.Option[Seq[String]] = None,
                               |    launchedOn: _root_.scala.Option[_root_.java.time.LocalDate] = None,
                               |    timestamp: _root_.scala.Option[_root_.java.time.Instant] = None""".stripMargin) should be(true)

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
                               |        Model(
                               |          guid = guid,
                               |          name = name,
                               |          __type__ = `type`,
                               |          curbWeight = curbWeight,
                               |          serial = serial,
                               |          finalDrive = finalDrive,
                               |          msrp = msrp,
                               |          isFlashy = isFlashy,
                               |          markets = markets,
                               |          launchedOn = launchedOn,
                               |          timestamp = timestamp
                               |        )
                               |      }
                               |    }""".stripMargin) should be(true)
  }

  it("Should produce models with primitives and no defaults when not required and when defaults are not in play") {
    val json = models.TestHelper.buildJson(requiredTrueDefaultNull)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Http4s018Generator.invoke(form)

    val jsOnly = res.filter(_.name.contains("ModelsJson")).head

    jsOnly.contents.contains("""case class Model(
                               |    guid: _root_.java.util.UUID,
                               |    name: String,
                               |    `type`: test.apidoc.apidoctest.v0.models.CarType,
                               |    curbWeight: Int,
                               |    serial: Long,
                               |    finalDrive: Double,
                               |    msrp: BigDecimal,
                               |    isFlashy: Boolean,
                               |    markets: Seq[String],
                               |    launchedOn: _root_.java.time.LocalDate,
                               |    timestamp: _root_.java.time.Instant
                               |  )""".stripMargin) should be(true)

    jsOnly.contents.contains("""for {
                               |        guid <- c.downField("guid").as[_root_.java.util.UUID]
                               |        name <- c.downField("name").as[String]
                               |        __type__ <- c.downField("type").as[test.apidoc.apidoctest.v0.models.CarType]
                               |        curbWeight <- c.downField("curb_weight").as[Int]
                               |        serial <- c.downField("serial").as[Long]
                               |        finalDrive <- c.downField("final_drive").as[Double]
                               |        msrp <- c.downField("msrp").as[BigDecimal]
                               |        isFlashy <- c.downField("is_flashy").as[Boolean]
                               |        markets <- c.downField("markets").as[Seq[String]]
                               |        launchedOn <- c.downField("launched_on").as[_root_.java.time.LocalDate]
                               |        timestamp <- c.downField("timestamp").as[_root_.java.time.Instant]
                               |      } yield {
                               |        Model(
                               |          guid = guid,
                               |          name = name,
                               |          __type__ = `type`,
                               |          curbWeight = curbWeight,
                               |          serial = serial,
                               |          finalDrive = finalDrive,
                               |          msrp = msrp,
                               |          isFlashy = isFlashy,
                               |          markets = markets,
                               |          launchedOn = launchedOn,
                               |          timestamp = timestamp
                               |        )
                               |      }
                               |    }""".stripMargin) should be(true)
  }
}
