package go.models

import Formatter._
import lib.{Datatype, DatatypeResolver}

case class ResponseBuilder(
  importBuilder: ImportBuilder,
  datatypeResolver: DatatypeResolver
) {

  def generate(readerName: String, datatype: Datatype): Option[String] = {
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
        // TODO: need to handle each type here
        Some(readerName)
      }

      case Datatype.Container.List(inner) => {
        val json = importBuilder.ensureImport("encoding/json")
        Some(
          Seq(
            s"var tmp ${goType.klass.localName}",
            s"${json}.NewDecoder($readerName).Decode(&tmp)",
            s"var all ${goType.klass.localName}",
            s"for _, el := range tmp {",
            s"append(all, ${generate(readerName, inner)})".indent(1),
            "}"
          ).mkString("\n")
        )
      }

      case Datatype.Container.Map(inner) => {
        None
      }

      case Datatype.UserDefined.Model(name) => {
        Some(s"${GoUtil.publicName(name)}FromJson($readerName)")
      }

      case Datatype.UserDefined.Union(name) => {
        Some(s"${GoUtil.publicName(name)}FromJson($readerName)")
      }

      case Datatype.UserDefined.Enum(name) => {
        Some(s"${GoUtil.publicName(name)}FromString($readerName)")
      }

      case Datatype.Container.Option(inner) => {
        generate(readerName, inner)
      }

    }
  }

}
