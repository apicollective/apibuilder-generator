package scala.models.play.files

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.generator.{ScalaEnum, ScalaModel, ScalaUnion}
import scala.models.play.Helpers.compareWithoutWhiteSpaces
import scala.models.play.gens.*

class ModelsBodyParsersSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

  private implicit val scalacheckConfig: PropertyCheckConfiguration = generatorDrivenConfig.copy(sizeRange = 10)

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
    forAll { enumDef: ScalaEnum =>
      val expected = ModelsBodyParsers.bodyParser(enumDef.name, enumDef.qualifiedName)
      val result = ModelsBodyParsers.bodyParser(enum)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates body parser for model based on name, and qualifiedName") {
    forAll { model: ScalaModel =>
      val expected = ModelsBodyParsers.bodyParser(model.name, model.qualifiedName)
      val result = ModelsBodyParsers.bodyParser(model)

      compareWithoutWhiteSpaces(result, expected)
    }
  }

  it("generates body parser for union based on name, and qualifiedName") {
    forAll { union: ScalaUnion =>
      val expected = ModelsBodyParsers.bodyParser(union.name, union.qualifiedName)
      val result = ModelsBodyParsers.bodyParser(union)

      compareWithoutWhiteSpaces(result, expected)
    }
  }
}
