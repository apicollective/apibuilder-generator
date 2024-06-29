package generator.elm

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import cats.data.ValidatedNec
import lib.Datatype
import lib.Datatype._

import scala.util.{Failure, Success}

sealed trait ElmType {
  def declaration: String
}

object ElmType {
  case object ElmString extends ElmType {
    override def declaration: String = "String"
  }
  case object ElmInt extends ElmType {
    override def declaration: String = "Int"
  }
  case object ElmBool extends ElmType {
    override def declaration: String = "Bool"
  }
  case object ElmFloat extends ElmType {
    override def declaration: String = "Float"
  }
  case object ElmDate extends ElmType {
    override def declaration: String = "Date"
  }
  case object ElmPosix extends ElmType {
    override def declaration: String = "Posix"
  }
  case object ElmNothing extends ElmType {
    override def declaration: String = "Nothing"
  }
  case class ElmEnumLocal(name: String) extends ElmType {
    assert(name == Names.pascalCase(name), "Name must be pascal case")
    override def declaration: String = name
  }
  case class ElmEnumImported(namespace: String, name: String) extends ElmType {
    assert(name == Names.pascalCase(name), "Name must be pascal case")
    override def declaration: String = name
  }
  case class ElmUserDefinedLocal(name: String) extends ElmType {
    assert(name == Names.pascalCase(name), "Name must be pascal case")
    override def declaration: String = name
  }
  case class ElmUserDefinedImported(namespace: String, name: String) extends ElmType {
    assert(name == Names.pascalCase(name), "Name must be pascal case")
    override def declaration: String = name
  }
  case class ElmList(typ: ElmType) extends ElmType {
    override def declaration: String = Util.maybeWrapInParens("List", typ.declaration)
  }
  case class ElmDict(typ: ElmType) extends ElmType {
    override def declaration: String = Util.maybeWrapInParens("Dict String", typ.declaration)
  }
  case class ElmMaybe(typ: ElmType) extends ElmType {
    override def declaration: String = Util.maybeWrapInParens("Maybe", typ.declaration)
  }
}

case class ElmTypeLookup(args: GenArgs) {

  def lookup(typ: String, required: Boolean): ElmType = {
    validate(typ, required = required) match {
      case Invalid(e) => sys.error(s"Failed to lookup type '$typ': ${e.toList.mkString(", ")}")
      case Valid(r) => r
    }
  }

  def validate(typ: String, required: Boolean): ValidatedNec[String, ElmType] = {
    args.datatypeResolver.parse(typ) match {
      case Failure(ex) => ex.getMessage.invalidNec
      case Success(t) => lookup(t).map { typ =>
        if (required) {
          typ
        } else {
          ElmType.ElmMaybe(typ)
        }
      }
    }
  }

  private def lookup(t: Datatype): ValidatedNec[String, ElmType] = {
    import ElmType._

    t match {
      case p: Primitive => {
        import lib.Datatype.Primitive._

        p match {
          case Boolean => ElmBool.validNec
          case Double => ElmFloat.validNec
          case Integer => ElmInt.validNec
          case Long => ElmInt.validNec
          case DateIso8601 => {
            args.imports.addExposing("Date", "Date")
            ElmDate.validNec
          }
          case DateTimeIso8601 => {
            args.imports.addExposing("Time", "Posix")
            ElmPosix.validNec
          }
          case Decimal => ElmFloat.validNec
          case Object => ElmString .validNec // TODO
          case JsonValue => ElmString.validNec // TODO
          case String => ElmString.validNec
          case Unit => ElmNothing .validNec // TODO Verify
          case Uuid => ElmString.validNec
        }
      }
      case u: UserDefined => {
        NamespaceParser.parse(u.name) match {
          case ParsedName.Local(n) => {
            val name = Names.pascalCase(n)
            u match {
              case _: UserDefined.Enum => ElmEnumLocal(name).validNec
              case _: UserDefined.Model | _: UserDefined.Union => ElmUserDefinedLocal(name).validNec
            }
          }
          case ParsedName.Imported(ns, n) => {
            val namespace = s"Generated.${Names.pascalCase(ns)}"
            val name = Names.pascalCase(n)
            args.imports.addExposingAll(namespace)
            u match {
              case _: UserDefined.Enum => ElmEnumImported(namespace, name).validNec
              case _: UserDefined.Model | _: UserDefined.Union => ElmUserDefinedImported(namespace, name).validNec
            }
          }
        }
      }
      case u: Generated.Model => s"TODO: Handle generated model: ${u.name}".invalidNec
      case u: Container.List => lookup(u.inner).map { v => ElmList(v) }
      case u: Container.Option => lookup(u.inner).map { v => ElmMaybe(v) }
      case u: Container.Map => {
        args.imports.addAs("Dict", "Dict")
        lookup(u.inner).map { v => ElmDict(v) }
      }
    }
  }
}
