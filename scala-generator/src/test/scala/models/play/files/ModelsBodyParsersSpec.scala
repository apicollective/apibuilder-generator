package scala.models.play.files

import io.apibuilder.spec.v0.models.{Enum, Model, Service, Union}
import org.scalatest.{FunSpec, Matchers}
import scala.models.play.Helpers.compareWithoutWhiteSpaces
import scala.generator.{ScalaEnum, ScalaModel, ScalaService, ScalaUnion}

class ModelsBodyParsersSpec extends FunSpec with Matchers {
  it("generates generic body parser") {
    val expected = s"""
      private def bodyParser[A](parser: ${ModelsBodyParsers.BodyParser}[${ModelsBodyParsers.JsValue}])(implicit ec: ${ModelsBodyParsers.ExecutionContext}, rds: ${ModelsBodyParsers.JsReads}[A]): ${ModelsBodyParsers.BodyParser}[A] =
        parser.validate(_.validate[A].asEither.left.map(e => ${ModelsBodyParsers.BadRequest}(${ModelsBodyParsers.JsError}.toJson(e))))
    """

    val result = ModelsBodyParsers.bodyParser()
    compareWithoutWhiteSpaces(expected, result)
  }

  it("generates body parser given a name, and type") {
    val name = "Foo"
    val tpe = "Bar"
    val expected = s"""
      def bodyParser${name}(parser: ${ModelsBodyParsers.BodyParser}[${ModelsBodyParsers.JsValue}])(implicit ec: ${ModelsBodyParsers.ExecutionContext}, rds: ${ModelsBodyParsers.JsReads}[${tpe}]): ${ModelsBodyParsers.BodyParser}[${tpe}] =
        bodyParser[${tpe}](parser)
    """

    val result = ModelsBodyParsers.bodyParser(name, tpe)

    compareWithoutWhiteSpaces(expected, result)
  }

  it("generates body parser for enum based on name, and qualifiedName") {
    val enum = new ScalaEnum(ScalaService(Service(null, "", null, null, "foo.bar", null, info = null)), Enum("Foo", null, values = Seq.empty))
    val expected = ModelsBodyParsers.bodyParser(enum)
    val result = ModelsBodyParsers.bodyParser(enum.name, enum.qualifiedName)

    compareWithoutWhiteSpaces(expected, result)
  }

  it("generates body parser for model based on name, and qualifiedName") {
    val model = new ScalaModel(ScalaService(Service(null, "", null, null, "foo.bar", null, info = null)), Model("Foo", "Foos", fields = Seq.empty))
    val expected = ModelsBodyParsers.bodyParser(model)
    val result = ModelsBodyParsers.bodyParser(model.name, model.qualifiedName)

    compareWithoutWhiteSpaces(expected, result)
  }

  it("generates body parser for union based on name, and qualifiedName") {
    val union = new ScalaUnion(ScalaService(Service(null, "", null, null, "foo.bar", null, info = null)), Union("Foo", null, types = Seq.empty))
    val expected = ModelsBodyParsers.bodyParser(union)
    val result = ModelsBodyParsers.bodyParser(union.name, union.qualifiedName)

    compareWithoutWhiteSpaces(expected, result)
  }
}
