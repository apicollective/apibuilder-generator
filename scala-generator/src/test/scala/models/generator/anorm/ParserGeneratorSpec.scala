package scala.generator.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import org.scalatest.{ShouldMatchers, FunSpec}

class ParserGeneratorSpec extends FunSpec with ShouldMatchers {

  val fileNames = Seq("TestApidocTestV1Conversions.scala", "TestApidocTestV1Parsers.scala")

  val referenceModel = """
    {
      "name": "reference",
      "plural": "references",
      "fields": [
        { "name": "guid", "type": "uuid", "required": true }
      ]
    }
  """

  val nameModel = """
    {
      "name": "name",
      "plural": "names",
      "fields": [
        { "name": "first", "type": "string", "required": false },
        { "name": "last", "type": "string", "required": false }
      ]
    }
  """

  case class ServiceBuilder(
    models: Seq[String] = Nil,
    enums: Seq[String] = Nil
  ) {

    def addModel(model: String): ServiceBuilder = {
      ServiceBuilder(
        models = this.models ++ Seq(model),
        enums = enums
      )
    }

    def addEnum(enum: String): ServiceBuilder = {
      ServiceBuilder(
        models = this.models,
        enums = this.enums ++ Seq(enum)
      )
    }

    lazy val json: String = {
      _root_.models.TestHelper.buildJson(s"""
        "imports": [],
        "headers": [],
        "info": [],
        "unions": [],
        "resources": [],
        "enums": [${enums.mkString(",\n")}],
        "models": [${models.mkString(",\n")}]
      """)
    }

    lazy val service = _root_.models.TestHelper.service(json)
    lazy val form = InvocationForm(service)

  }

  it("service with no models") {
    val form = ServiceBuilder().form
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        // Success
      }
      case Right(files) => {
        fail("Expected an error message for a service w/ no models: " + files.last.contents)
      }
    }
  }

  it("model with one field") {
    val form = ServiceBuilder(models = Seq(referenceModel)).form
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/conversions.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/reference.txt", files.last.contents)
      }
    }
  }

  it("model with multiple fields") {
    val form = ServiceBuilder(models = Seq(nameModel)).form
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/conversions.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/name.txt", files.last.contents)
      }
    }
  }

  it("composite model") {
    val form = ServiceBuilder(models = Seq(nameModel)).addModel("""
      {
        "name": "user",
        "plural": "users",
        "fields": [
          { "name": "guid", "type": "uuid", "required": true },
          { "name": "email", "type": "string", "required": true },
          { "name": "name", "type": "name", "required": false }
        ]
      }
    """).form

    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/conversions.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/user.txt", files.last.contents)
      }
    }
  }

  it("enum model") {
    val form = ServiceBuilder().addModel("""
    {
      "name": "user",
      "plural": "users",
      "fields": [
        { "name": "guid", "type": "uuid", "required": true },
        { "name": "status", "type": "status", "required": true }
      ]
    }
    """).addEnum("""
    {
      "name": "status",
      "plural": "statuses",
      "values": [
        { "name": "active" },
        { "name": "inactive" }
      ]
    }
    """).form

    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/conversions.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/enum.txt", files.last.contents)
      }
    }
  }

  it("model with list field") {
    val form = ServiceBuilder().addModel("""
    {
      "name": "user",
      "plural": "users",
      "fields": [
        { "name": "guid", "type": "uuid", "required": true },
        { "name": "emails", "type": "[string]", "required": true, "default": "[]" }
      ]
    }
    """).form

    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(fileNames)
        models.TestHelper.assertEqualsFile("/generator/anorm/conversions.txt", files.head.contents)
        models.TestHelper.assertEqualsFile("/generator/anorm/list.txt", files.last.contents)
      }
    }
  }

}
