package scala.generator.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import org.scalatest.{ShouldMatchers, FunSpec}

class ParserGeneratorSpec extends FunSpec with ShouldMatchers {

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

  val userModel = """
    {
      "name": "user",
      "plural": "users",
      "fields": [
        { "name": "guid", "type": "uuid", "required": true },
        { "name": "email", "type": "string", "required": true },
        { "name": "name", "type": "name", "required": false }
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

    def build(): String = {
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

  }

  it("service with no models") {
    val json = ServiceBuilder().build()
    val form = InvocationForm(models.TestHelper.service(json))
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        // Success
      }
      case Right(files) => {
        fail("Expected an error message for a service w/ no models: " + files.head.contents)
      }
    }
  }

  it("model with one field") {
    val json = ServiceBuilder(models = Seq(referenceModel)).build()

    val form = InvocationForm(models.TestHelper.service(json))
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(Seq("TestApidocTestV1Parsers.scala"))
        models.TestHelper.assertEqualsFile("/generator/anorm/reference.txt", files.head.contents)
      }
    }
  }

  it("model with multiple fields") {
    val json = ServiceBuilder(models = Seq(nameModel)).build()

    val form = InvocationForm(models.TestHelper.service(json))
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(Seq("TestApidocTestV1Parsers.scala"))
        models.TestHelper.assertEqualsFile("/generator/anorm/name.txt", files.head.contents)
      }
    }
  }

  it("composite model") {
    val json = ServiceBuilder(models = Seq(nameModel, userModel)).build()

    val form = InvocationForm(models.TestHelper.service(json))
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(Seq("TestApidocTestV1Parsers.scala"))
        models.TestHelper.assertEqualsFile("/generator/anorm/user.txt", files.head.contents)
      }
    }
  }

}
