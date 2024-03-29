package go.models

import Formatter._
import lib.{Datatype, DatatypeResolver}

object ResponseBuilder {

  sealed trait DataSource
  case object FromJson extends DataSource
  case object FromMap extends DataSource

}

case class ResponseBuilder(
  importBuilder: ImportBuilder,
  datatypeResolver: DatatypeResolver
) {

  def generate(readerName: String, datatype: Datatype, deserializer: ResponseBuilder.DataSource): Option[String] = {
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

      case Datatype.Primitive.JsonValue => {
        val json = importBuilder.ensureImport("encoding/json")
        Some(
          Seq(
            s"var tmp ${goType.klass.localName}",
            s"${json}.NewDecoder($readerName).Decode(&tmp)",
            "tmp"
          ).mkString("\n")
        )
      }

      case Datatype.Primitive.String => {
        deserializer match {
          case ResponseBuilder.FromMap =>
            Some(
              s"$readerName.(string)"
            )
          case ResponseBuilder.FromJson =>
            val ioutil = importBuilder.ensureImport("io/ioutil")
            Some(
              Seq(
                s"func() ${goType.klass.localName} {",
                Seq(
                  s"body, _ := ${ioutil}.ReadAll($readerName)",
                  "return string(body)"
                ).mkString("\n").indentString(1),
                "}()"
              ).mkString("\n")
            )
        }
      }

      case _: Datatype.Primitive => {
        // TODO: need to handle all the primitive types here
        Some(s"string($readerName)")
      }

      case Datatype.Container.List(inner) => {
        val json = importBuilder.ensureImport("encoding/json")
        Some(
          Seq(
            s"func() ${goType.klass.localName} {",
            Seq(
              s"var tmp []interface{}",
              s"${json}.NewDecoder($readerName).Decode(&tmp)",
              s"var all ${goType.klass.localName}",
              s"for _, el := range tmp {",
              generate("el", inner, ResponseBuilder.FromMap) match {
                case None => "// no-op as type is nil".indentString(1)
                case Some(code) => s"all = append(all, $code)".indentString(1)
              },
              "}",
              "return all"
            ).mkString("\n").indentString(1),
            "}()"
          ).mkString("\n")
        )
      }

      case Datatype.Container.Map(inner) => {
        val json = importBuilder.ensureImport("encoding/json")
        Some(
          Seq(
            s"func() ${goType.klass.localName} {",
            Seq(
              s"var tmp map[string]interface{}",
              s"${json}.NewDecoder($readerName).Decode(&tmp)",
              s"var all ${goType.klass.localName}",
              s"for key, el := range tmp {",
              generate("el", inner, ResponseBuilder.FromMap) match {
                case None => "// no-op as type is nil".indentString(1)
                case Some(code) => s"all[key] = $code".indentString(1)
              },
              "}",
              "return all"
            ).mkString("\n").indentString(1),
            "}()"
          ).mkString("\n")
        )
      }

      case Datatype.Generated.Model(_) => {
        sys.error("Generated types should not be available in responses")
      }

      case Datatype.UserDefined.Model(name) => {
        Some(s"${importBuilder.publicName(name)}$deserializer($readerName)")
      }

      case Datatype.UserDefined.Union(name) => {
        Some(s"${importBuilder.publicName(name)}$deserializer($readerName)")
      }

      case Datatype.UserDefined.Enum(name) => {
        Some(
          deserializer match {
            case ResponseBuilder.FromJson => s"${importBuilder.publicName(name)}FromString(string($readerName))"
            case ResponseBuilder.FromMap => s"""${importBuilder.publicName(name)}FromString($readerName["value"].(string))"""
          }
        )
      }

      case Datatype.Container.Option(inner) => {
        generate(readerName, inner, deserializer)
      }

    }
  }

}
