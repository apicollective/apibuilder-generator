package models.generator
import io.apibuilder.generator.v0.models.InvocationForm
import models.FieldDefaultHelper._
import org.scalatest.{FunSpec, Matchers}

import scala.models.Play2StandaloneModelsJson

class ScalaDefaultsSpec extends FunSpec with Matchers {
  it("Should produce models with defaults and scala_generator.model_hint missing") {
    val json = models.TestHelper.buildJson(requiredFalseWithDefaultsNoHints)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Play2StandaloneModelsJson.invoke(form)

    val jsOnly = res.head

    jsOnly.contents.contains("""  final case class Model(
                               |    guid: _root_.java.util.UUID = _root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd"),
                               |    name: String = "M3",
                               |    `type`: test.apidoc.apidoctest.v0.models.CarType = test.apidoc.apidoctest.v0.models.CarType.Coupe,
                               |    curbWeight: Int = 3500,
                               |    serial: Long = 45678901234L,
                               |    finalDrive: Double = 3.85,
                               |    msrp: BigDecimal = 45999.99,
                               |    isFlashy: Boolean = true,
                               |    markets: Seq[String] = scala.List("USA","CAN"),
                               |    launchedOn: _root_.org.joda.time.LocalDate = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate("1986-02-01"),
                               |    timestamp: _root_.org.joda.time.DateTime = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime("2018-03-21T02:20:52Z")
                               |  )""".stripMargin) should be(true)
  }

  it("Should produce models with defaults and scala_generator.model_hint == required") {
    val json = models.TestHelper.buildJson(requiredFalseWithDefaultsHintDeveloperFriendly)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Play2StandaloneModelsJson.invoke(form)

    val jsOnly = res.head

    jsOnly.contents.contains("""  final case class Model(
                               |    guid: _root_.java.util.UUID = _root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd"),
                               |    name: String = "M3",
                               |    `type`: test.apidoc.apidoctest.v0.models.CarType = test.apidoc.apidoctest.v0.models.CarType.Coupe,
                               |    curbWeight: Int = 3500,
                               |    serial: Long = 45678901234L,
                               |    finalDrive: Double = 3.85,
                               |    msrp: BigDecimal = 45999.99,
                               |    isFlashy: Boolean = true,
                               |    markets: Seq[String] = scala.List("USA","CAN"),
                               |    launchedOn: _root_.org.joda.time.LocalDate = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate("1986-02-01"),
                               |    timestamp: _root_.org.joda.time.DateTime = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime("2018-03-21T02:20:52Z")
                               |  )""".stripMargin) should be(true)
  }

  it("Should produce models with defaults and scala_generator . model_hint == optional") {
    val json = models.TestHelper.buildJson(requiredFalseWithDefaultsHintWireFriendly)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Play2StandaloneModelsJson.invoke(form)

    val jsOnly = res.head

    jsOnly.contents.contains("""  final case class Model(
                               |    guid: _root_.scala.Option[_root_.java.util.UUID] = Some(_root_.java.util.UUID.fromString("abcd-ef01-2345-6789-abcd")),
                               |    name: _root_.scala.Option[String] = Some("M3"),
                               |    `type`: _root_.scala.Option[test.apidoc.apidoctest.v0.models.CarType] = Some(test.apidoc.apidoctest.v0.models.CarType.Coupe),
                               |    curbWeight: _root_.scala.Option[Int] = Some(3500),
                               |    serial: _root_.scala.Option[Long] = Some(45678901234L),
                               |    finalDrive: _root_.scala.Option[Double] = Some(3.85),
                               |    msrp: _root_.scala.Option[BigDecimal] = Some(45999.99),
                               |    isFlashy: _root_.scala.Option[Boolean] = Some(true),
                               |    markets: _root_.scala.Option[Seq[String]] = Some(scala.List("USA","CAN")),
                               |    launchedOn: _root_.scala.Option[_root_.org.joda.time.LocalDate] = Some(_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate("1986-02-01")),
                               |    timestamp: _root_.scala.Option[_root_.org.joda.time.DateTime] = Some(_root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime("2018-03-21T02:20:52Z"))
                                |  )""".stripMargin) should be(true)
  }

  it("Should produce models wrapped with Option set by default to None when defaults are not in play") {
    val json = models.TestHelper.buildJson(requiredFalseDefaultNull)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Play2StandaloneModelsJson.invoke(form)

    val jsOnly = res.head

    jsOnly.contents.contains("""  final case class Model(
                               |    guid: _root_.scala.Option[_root_.java.util.UUID] = None,
                               |    name: _root_.scala.Option[String] = None,
                               |    `type`: _root_.scala.Option[test.apidoc.apidoctest.v0.models.CarType] = None,
                               |    curbWeight: _root_.scala.Option[Int] = None,
                               |    serial: _root_.scala.Option[Long] = None,
                               |    finalDrive: _root_.scala.Option[Double] = None,
                               |    msrp: _root_.scala.Option[BigDecimal] = None,
                               |    isFlashy: _root_.scala.Option[Boolean] = None,
                               |    markets: _root_.scala.Option[Seq[String]] = None,
                               |    launchedOn: _root_.scala.Option[_root_.org.joda.time.LocalDate] = None,
                               |    timestamp: _root_.scala.Option[_root_.org.joda.time.DateTime] = None
                               |  )""".stripMargin) should be(true)
  }



  it("Should produce models with primitives and no defaults when not required and when defaults are not in play") {
    val json = models.TestHelper.buildJson(
      requiredTrueDefaultNull)

    val form = InvocationForm(models.TestHelper.service(json.format()))

    val Right(res) = Play2StandaloneModelsJson.invoke(form)

    val jsOnly = res.head

    jsOnly.contents.contains("""  final case class Model(
                               |    guid: _root_.java.util.UUID,
                               |    name: String,
                               |    `type`: test.apidoc.apidoctest.v0.models.CarType,
                               |    curbWeight: Int,
                               |    serial: Long,
                               |    finalDrive: Double,
                               |    msrp: BigDecimal,
                               |    isFlashy: Boolean,
                               |    markets: Seq[String],
                               |    launchedOn: _root_.org.joda.time.LocalDate,
                               |    timestamp: _root_.org.joda.time.DateTime
                               |  )""".stripMargin) should be(true)
  }
}
