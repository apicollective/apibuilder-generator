package generator.elm

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import cats.data.ValidatedNec
import lib.Datatype
import lib.Datatype._

import scala.util.{Failure, Success}

case class ElmType(args: GenArgs) {

  def lookup(typ: String): String = {
    validate(typ) match {
      case Invalid(e) => sys.error(s"Failed to lookup type '$typ': ${e.toList.mkString(", ")}")
      case Valid(r) => r
    }
  }

  def validate(typ: String): ValidatedNec[String, String] = {
    args.datatypeResolver.parse(typ) match {
      case Failure(ex) => ex.getMessage.invalidNec
      case Success(t) => lookup(t)
    }
  }

  private[this] def lookup(t: Datatype): ValidatedNec[String, String] = {
    t match {
      case p: Primitive => {
        import lib.Datatype.Primitive._

        p match {
          case Boolean => "Bool".validNec
          case Double => "Float".validNec
          case Integer => "Int".validNec
          case Long => "Int".validNec
          case DateIso8601 => {
            args.imports.addExposing("Date", "Date")
            "Date".validNec
          }
          case DateTimeIso8601 => {
            args.imports.addExposing("Time", "Posix")
            "Posix".validNec
          }
          case Decimal => "Float".validNec
          case Object => "String" .validNec // TODO
          case JsonValue => "String".validNec // TODO
          case String => "String".validNec
          case Unit => "Nothing" .validNec // TODO Verify
          case Uuid => "String".validNec
        }
      }
      case u: UserDefined => {
        NamespaceParser.parse(u.name) match {
          case ParsedName.Local(name) => Names.pascalCase(name).validNec
          case ParsedName.Imported(namespace, name) => {
            args.imports.addExposingAll(s"Generated.${Names.pascalCase(namespace)}")
            Names.pascalCase(name).validNec
          }
        }
      }
      case u: Generated.Model => s"TODO: Handle generated model: ${u.name}".invalidNec
      case u: Container.List => lookup(u.inner).map { v => Util.maybeWrapInParens("List", v) }
      case u: Container.Option => lookup(u.inner).map { v => Util.maybeWrapInParens("Maybe", v) }
      case u: Container.Map => {
        args.imports.addAs("Dict", "Dict")
        lookup(u.inner).map { v => Util.maybeWrapInParens("Dict String", v) }
      }
    }
  }
}
