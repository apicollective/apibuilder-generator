package models.generator.anorm

import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}

import scala.generator.anorm.ParserGenerator26
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ParserGenerator26Spec extends AnyFunSpec with Matchers {

  val fileNames = Seq("TestApidocTestV1Conversions.scala", "TestApidocTestV1Parsers.scala")

  val referenceModel = """
    {
      "name": "reference",
      "plural": "references",
      "attributes": [],
      "fields": [
        { "name": "guid", "type": "uuid", "required": true, "attributes": [] }
      ]
    }
  """

  val dateTimeModel = """
    {
      "name": "reference",
      "plural": "references",
      "attributes": [],
      "fields": [
        { "name": "date", "type": "date-iso8601", "required": true, "attributes": [] },
        { "name": "time", "type": "date-time-iso8601", "required": true, "attributes": [] }
      ]
    }
  """

  val nameModel = """
    {
      "name": "name",
      "plural": "names",
      "attributes": [],
      "fields": [
        { "name": "first", "type": "string", "required": false, "attributes": [] },
        { "name": "last", "type": "string", "required": false, "attributes": [] }
      ]
    }
  """

  val capModel = """
    {
      "name": "name",
      "plural": "names",
      "attributes": [],
      "fields": [
        { "name": "First", "type": "string", "required": false, "attributes": [] },
        { "name": "Last", "type": "string", "required": false, "attributes": [] }
      ]
    }
  """

  case class ServiceBuilder(
    unions: Seq[String] = Nil,
    models: Seq[String] = Nil,
    enums: Seq[String] = Nil
  ) {

    def addUnion(union: String): ServiceBuilder = {
      ServiceBuilder(
        unions = this.unions ++ Seq(union),
        models = this.models,
        enums = this.enums
      )
    }

    def addModel(model: String): ServiceBuilder = {
      ServiceBuilder(
        unions = this.unions,
        models = this.models ++ Seq(model),
        enums = enums
      )
    }

    def addEnum(`enum`: String): ServiceBuilder = {
      ServiceBuilder(
        unions = this.unions,
        models = this.models,
        enums = this.enums ++ Seq(enum)
      )
    }

    lazy val json: String = {
      _root_.models.TestHelper.buildJson(s"""
        "imports": [],
        "headers": [],
        "info": [],
        "resources": [],
        "unions": [${unions.mkString(",\n")}],
        "enums": [${enums.mkString(",\n")}],
        "models": [${models.mkString(",\n")}],
        "attributes": []
      """)
    }

    lazy val service = _root_.models.TestHelper.service(json)
    lazy val form = InvocationForm(service)

  }

  it("service with no models") {
    val form = ServiceBuilder().form
    ParserGenerator26.invoke(form) match {
      case Left(_) => {
        // Success
      }
      case Right(files) => {
        fail("Expected an error message for a service w/ no models: " + files.last.contents)
      }
    }
  }

  it("model with one field") {
    val form = ServiceBuilder(models = Seq(referenceModel)).form
    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/reference-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/reference.txt", files.last.contents)
      }
    }
  }

  it("model with multiple fields") {
    val form = ServiceBuilder(models = Seq(nameModel)).form
    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/name-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/name.txt", files.last.contents)
      }
    }
  }

  it("model with capitalized fields") {
    val form = ServiceBuilder(models = Seq(capModel)).form
    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/cap-name-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/cap-name.txt", files.last.contents)
      }
    }
  }

  it("composite model") {
    val form = ServiceBuilder(models = Seq(nameModel)).addModel("""
      {
        "name": "user",
        "plural": "users",
        "attributes": [],
        "fields": [
          { "name": "guid", "type": "uuid", "required": true, "attributes": [] },
          { "name": "email", "type": "string", "required": true, "attributes": [] },
          { "name": "name", "type": "name", "required": false, "attributes": [] },
          { "name": "obj", "type": "object", "required": false, "attributes": [] },
          { "name": "blob", "type": "json", "required": false, "attributes": [] }
        ]
      }
    """).form

    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/user-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/user.txt", files.last.contents)
      }
    }
  }

  it("enum model") {
    val form = ServiceBuilder().addModel("""
    {
      "name": "user",
      "plural": "users",
      "attributes": [],
      "fields": [
        { "name": "guid", "type": "uuid", "required": true, "attributes": [] },
        { "name": "status", "type": "status", "required": true, "attributes": [] }
      ]
    }
    """).addEnum("""
    {
      "name": "status",
      "plural": "statuses",
      "attributes": [],
      "values": [
        { "name": "active", "attributes": [] },
        { "name": "inactive", "attributes": [] }
      ]
    }
    """).form

    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/enum-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/enum.txt", files.last.contents)
      }
    }
  }

  it("model with list field") {
    val form = ServiceBuilder().addModel("""
    {
      "name": "user",
      "plural": "users",
      "attributes": [],
      "fields": [
        { "name": "guid", "type": "uuid", "required": true, "attributes": [] },
        { "name": "emails", "type": "[string]", "required": true, "default": "[]", "attributes": [] }
      ]
    }
    """).form

    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/list-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/list.txt", files.last.contents)
      }
    }
  }

  it("with union type") {
    val form = ServiceBuilder().addModel("""
    {
      "name": "guest_user",
      "plural": "guest_users",
      "attributes": [],
      "fields": [
        { "name": "guid", "type": "uuid", "required": true, "attributes": [] }
      ]
    }
    """).addModel("""
    {
      "name": "registered_user",
      "plural": "registered_users",
      "attributes": [],
      "fields": [
        { "name": "guid", "type": "uuid", "required": true, "attributes": [] }
      ]
    }
    """).addUnion("""
    {
      "name": "user",
      "plural": "users",
      "attributes": [],
      "types": [
        { "type": "guest_user", "attributes": [] },
        { "type": "registered_user", "attributes": [] }
      ]
    }
    """).form

    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/union-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/union-parsers.txt", files.last.contents)
      }
    }
  }

  it("model with fields that are composite names") {
    val form = ServiceBuilder().addModel("""
    {
      "name": "location",
      "plural": "locations",
      "attributes": [],
      "fields": [
        { "name": "ip_address", "type": "string", "required": true, "attributes": [] }
      ]
    }
    """).form

    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/location-conversions-26.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/location-parsers.txt", files.last.contents)
      }
    }
  }

  it("anorm bindings for joda time types") {
    val form = ServiceBuilder(models = Seq(dateTimeModel)).form
    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/datetime-conversions-joda.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/datetime-joda.txt", files.last.contents)
      }
    }
  }

  it("anorm bindings for java time types") {
    val form = ServiceBuilder(models = Seq(dateTimeModel)).form.copy(attributes = Seq(Attribute("scala_generator.time_library", "java")))
    ParserGenerator26.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/datetime-conversions-java.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/datetime-java.txt", files.last.contents)
      }
    }
  }

}
