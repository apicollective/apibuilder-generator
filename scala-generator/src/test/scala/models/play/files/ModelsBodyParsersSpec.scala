package scala.models.play.files

import io.apibuilder.spec.v0.models.{Enum, Model, Service, Union}
import io.apibuilder.spec.v0.models.gens._
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scala.models.play.gens._
import scala.models.play.Helpers.compareWithoutWhiteSpaces
import scala.generator.{ScalaEnum, ScalaModel, ScalaService, ScalaUnion}

class ModelsBodyParsersSpec extends FunSpec with Matchers with PropertyChecks {

  implicit val scalacheckConfig = generatorDrivenConfig.copy(sizeRange = 10)

  it("generates generic body parser") {
    val expected = s"""
      private def bodyParser[A](parser: ${ModelsBodyParsers.BodyParser}[${ModelsBodyParsers.JsValue}])(implicit ec: ${ModelsBodyParsers.ExecutionContext}, rds: ${ModelsBodyParsers.JsReads}[A]): ${ModelsBodyParsers.BodyParser}[A] =
        parser.validate(_.validate[A].asEither.left.map(e => ${ModelsBodyParsers.BadRequest}(${ModelsBodyParsers.JsError}.toJson(e))))
    """

    val result = ModelsBodyParsers.bodyParser()
    compareWithoutWhiteSpaces(result, expected)
  }

  it("generates body parser given a name, and type") {
    forAll { (name: String, tpe: String) =>
      val expected = s"""
        def bodyParser${name}(parser: ${ModelsBodyParsers.BodyParser}[${ModelsBodyParsers.JsValue}])(implicit ec: ${ModelsBodyParsers.ExecutionContext}, rds: ${ModelsBodyParsers.JsReads}[${tpe}]): ${ModelsBodyParsers.BodyParser}[${tpe}] =
          bodyParser[${tpe}](parser)
      """

      val result = ModelsBodyParsers.bodyParser(name, tpe)
      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates body parser for enum based on name, and qualifiedName") {
    forAll { enum: ScalaEnum =>
      val expected = ModelsBodyParsers.bodyParser(enum)
      val result = ModelsBodyParsers.bodyParser(enum.name, enum.qualifiedName)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates body parser for model based on name, and qualifiedName") {
    forAll { model: ScalaModel =>
      val expected = ModelsBodyParsers.bodyParser(model)
      val result = ModelsBodyParsers.bodyParser(model.name, model.qualifiedName)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates body parser for union based on name, and qualifiedName") {
    forAll { union: ScalaUnion =>
      val expected = ModelsBodyParsers.bodyParser(union)
      val result = ModelsBodyParsers.bodyParser(union.name, union.qualifiedName)

      compareWithoutWhiteSpaces(result, expected)
    }
  }
}
