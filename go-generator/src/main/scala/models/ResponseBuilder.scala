package go.models

import Formatter._
import lib.{Datatype, DatatypeResolver}

case class ResponseBuilder(
  importBuilder: ImportBuilder,
  datatypeResolver: DatatypeResolver
) {
  private[this] val FromJson = "FromJson"
  private[this] val FromMap = "FromMap"

  def generate(readerName: String, datatype: Datatype): Option[String] = {
    generate(readerName, datatype, FromJson)
  }

  private[this] def generate(readerName: String, datatype: Datatype, deserializer: String): Option[String] = {
    val goType = GoType(importBuilder, datatype)

    datatype match {
      case Datatype.Primitive.Unit => {
        None
      }

      case Datatype.Primitive.Object => {
        val json = importBuilder.ensureImport("encoding/json")
        Some(
          Seq(
            s"var tmp ${goType.klass.localName}",
            s"${json}.NewDecoder($readerName).Decode(&tmp)",
            "tmp"
          ).mkString("\n")
        )
      }

      case t: Datatype.Primitive => {
        // TODO: need to handle all the primitive types here
        Some(s"string($readerName)")
      }

      case Datatype.Container.List(inner) => {
        val json = importBuilder.ensureImport("encoding/json")
        Some(
          Seq(
            s"func() ${goType.klass.localName} {",
            Seq(
              s"var tmp ${goType.klass.localName}",
              s"${json}.NewDecoder($readerName).Decode(&tmp)",
              s"var all ${goType.klass.localName}",
              s"for _, el := range tmp {",
              generate("el", inner, FromMap) match {
                case None => "// no-op as type is nil".indent(1)
                case Some(code) => s"all = append(all, $code)".indent(1)
              },
              "}",
              "return all"
            ).mkString("\n").indent(1),
            "}()"
          ).mkString("\n")
        )
      }

      case Datatype.Container.Map(inner) => {
        None
      }

      case Datatype.UserDefined.Model(name) => {
        Some(s"${GoUtil.publicName(name)}$deserializer($readerName)")
      }

      case Datatype.UserDefined.Union(name) => {
        Some(s"${GoUtil.publicName(name)}$deserializer($readerName)")
      }

      case Datatype.UserDefined.Enum(name) => {
        Some(
          deserializer match {
            case FromJson => s"${GoUtil.publicName(name)}FromString(string($readerName))"
            case FromMap => s"""${GoUtil.publicName(name)}FromString($readerName["value"].(string))"""
          }
        )
      }

      case Datatype.Container.Option(inner) => {
        generate(readerName, inner, deserializer)
      }

    }
  }

}
