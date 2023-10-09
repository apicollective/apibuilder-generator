package generator.elm

import lib.Datatype
import lib.Datatype._

import scala.util.{Failure, Success}

case class ElmType(args: GenArgs) {

  def lookup(typ: String): String = {
    args.datatypeResolver.parse(typ) match {
      case Failure(ex) => throw ex
      case Success(t) => lookup(t)
    }
  }

  private[this] def lookup(t: Datatype): String = {
    t match {
      case p: Primitive => {
        import lib.Datatype.Primitive._

        p match {
          case Boolean => "Bool"
          case Double => "Float"
          case Integer => "Int"
          case Long => "Int"
          case DateIso8601 => "String"
          case DateTimeIso8601 => "String"
          case Decimal => "Float"
          case Object => "String" // TODO
          case JsonValue => "String" // TODO
          case String => "String"
          case Unit => "Nothing" // TODO Verify
          case Uuid => "String"
        }
      }
      case u: UserDefined => {
        NamespaceParser.parse(u.name) match {
          case ParsedName.Local(name) => Names.pascalCase(name)
          case ParsedName.Imported(namespace, name) => {
            args.imports.addExposingAll(s"Generated.${Names.pascalCase(namespace)}")
            Names.pascalCase(name)
          }
        }
      }
      case u: Generated.Model => sys.error(s"TODO: Handle generated model: ${u.name}")
      case u: Container.List => "List (" + lookup(u.inner) + ")"
      case _: Container.Option => sys.error("Not sure hot to handle option yet")
      case u: Container.Map => "Dict String " + lookup(u.inner)
    }
  }
}
