package generator.elm

import io.apibuilder.spec.v0.models.{Field, Model}
import lib.Datatype
import lib.Datatype.{Container, Generated, Primitive, UserDefined}

import scala.util.{Failure, Success}

case class ElmModel(args: GenArgs) {
  private[this] val elmJson = ElmJson(args.imports)
  private[this] val elmType = ElmType(args)

  def generate(model: Model): String = {
    Seq(
      genTypeAlias(model),
      genEncoder(model),
      //genDecoder(model)
    ).mkString("\n\n")
  }

  private[this] def genTypeAlias(model: Model): String = {
    Seq(
      s"type alias ${Names.pascalCase(model.name)} =",
      "  {",
      model.fields.map { f =>
        val opt = if (f.required) {
          ""
        } else {
          "Maybe "
        }
        s"${Names.camelCase(f.name)}: $opt${elmType.lookup(f.`type`)}"
      }.mkString("\n, ").indent(4).stripTrailing(),
      "  }"
    ).mkString("\n")
  }


  private[this] def genEncoder(m: Model): String = {
    args.imports.addAs("Json.Encode", "Encode")
    elmJson.encoder(m.name) {
      Seq(
        "Encode.object",
        "[",
        m.fields.zipWithIndex.map { case (f,i) =>
          val encoder = args.datatypeResolver.parse(f.`type`) match {
            case Success(v) => v match {
              case p: Datatype.Primitive => fieldEncoder(f, primitiveEncoder(p))
              case u: UserDefined => {
                val name = NamespaceParser.parse(u.name) match {
                  case ParsedName.Local(name) => Names.camelCase(name)
                  case ParsedName.Imported(namespace, name) => {
                    args.imports.addExposingAll(s"Generated.${Names.pascalCase(namespace)}")
                    Names.camelCase(name)
                  }
                }
                fieldEncoder(f, name + "Encoder")
              }
              case m: Generated.Model => {
                fieldEncoder(f, Names.camelCase(m.name) + "Encoder")
              }
              case u: Container.List => {
                println(s"model ${m.name} Field ${f.name} has type list: ${u.name}")
              }
              case u: Container.Option => {
                println(s"model ${m.name} Field ${f.name} has type option: ${u.name}")
              }
              case u: Container.Map => {
                println(s"model ${m.name} Field ${f.name} has type map: ${u.name}")
              }
            }
            case Failure(ex) => throw ex
          }
          if (i == 0) {
            encoder
          } else {
            s", " + encoder
          }
        }.mkString("\n").indent(4),
        "]"
      ).mkString("\n").indent(4)
    }
  }

  private[this] def primitiveEncoder(p: Primitive): String = {
    args.imports.addAs("Json.Encode", "Encode")
    import Datatype.Primitive._
    p match {
      case Boolean => "Encode.boolean"
      case Double => "Encode.float"
      case Integer => "Encode.int"
      case Long => "Encode.int"
      case DateIso8601 => "Encode.string"
      case DateTimeIso8601 => "Encode.string"
      case Decimal => "Encode.float"
      case Object => "Encode.string" // TODO
      case JsonValue => "Encode.string" // TODO
      case String => "Encode.string"
      case Unit => "(Encode.success Nothing)" // TODO Verify
      case Uuid => "Encode.string"
    }
  }

  private[this] def fieldEncoder(f: Field, encoder: String): String = {
    if (f.required) {
      s"""( ${Names.wrapInQuotes(f.name)}, $encoder instance.${Names.camelCase(f.name)} )"""
    } else {
      s"""( ${Names.wrapInQuotes(f.name)}, encodeOptional $encoder instance.${Names.camelCase(f.name)} )"""
    }
  }

  /*
  memberStatusDecoder : Decoder MemberStatus
  memberStatusDecoder =
      Decode.map memberStatusFromString string
   */
  private[this] def genDecoder(m: Model): String = {
    elmJson.decoder(m.name) {
      m.fields.map { f =>

      }.mkString("\n").indent(4)
    }
  }
}
