package scala.generator.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import org.scalatest.{ShouldMatchers, FunSpec}

class ParserGeneratorSpec extends FunSpec with ShouldMatchers {

  def buildJsonFromModels(value: String): String = {
    models.TestHelper.buildJson(s"""
      "imports": [],
      "headers": [],
      "info": [],
      "models": [],
      "enums": [],
      "unions": [],
      "resources": [],

      "models": [
        {
          "name": "reference",
          "plural": "references",
          "fields": [
            { "name": "guid", "type": "uuid", "required": true }
          ]
        }
      ]
    """)
  }

  it("model with one field") {
    val json = buildJsonFromModels("""
        {
          "name": "reference",
          "plural": "references",
          "fields": [
            { "name": "guid", "type": "uuid", "required": true }
          ]
        }
     """)

    val form = InvocationForm(models.TestHelper.service(json))
    ParserGenerator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(Seq("Parsers.scala"))
        models.TestHelper.assertEqualsFile("/generator/anorm/reference.txt", files.head.contents)
      }
    }
  }

}
