package go.models

import Formatter._
import io.apibuilder.spec.v0.models.Parameter
import lib.{Datatype, DatatypeResolver}

case class UrlValues(
  importBuilder: ImportBuilder,
  datatypeResolver: DatatypeResolver
) {

  def generate(prefix: String, params: Seq[Parameter]): Option[String] = {
    params match {
      case Nil => None
      case _ => {
        val url = importBuilder.ensureImport("net/url")
        Some(
          Seq(
	    s"urlValues := ${url}.Values{}",
            params.map { p =>
              buildParam(prefix, p, datatype(p.`type`, p.required))
            }.mkString("\n")
          ).mkString("\n")
        )
      }
    }
  }

  private[this] def buildParam(prefix: String, param: Parameter, datatype: Datatype): String = {
    val goType = GoType(importBuilder, datatype)
    val varName = s"${prefix}." + GoUtil.publicName(param.name)

    goType.datatype match {
      case Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long | Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid | Datatype.Primitive.Boolean | Datatype.Primitive.Object | Datatype.Primitive.JsonValue | Datatype.Primitive.Unit | Datatype.UserDefined.Enum(_) => {
        param.default match {
          case None => {
            Seq(
              "if " + goType.notNil(varName) + " {",
              build(param.name, varName, goType).indentString(1),
              "}"
            ).mkString("\n")
          }
          case Some(default) => {
            Seq(
              "if " + goType.nil(varName) + " {",
              GoType.isNumeric(datatype) match {
                case true => {
                  build(param.name, default, goType).indentString(1)
                }
                case false => {
                  build(param.name, GoUtil.wrapInQuotes(default), goType).indentString(1)
                }
              },
              "} else {",
              build(param.name, varName, goType).indentString(1),
              "}"
            ).mkString("\n")
          }
        }
      }
      case Datatype.Container.List(_) => {
        Seq(
          s"for _, value := range $varName {",
          build(param.name, "value", goType).indentString(1),
          "}"
        ).mkString("\n")
      }

      case Datatype.Container.Option(inner) => {
        buildParam(prefix, param, inner)
      }

      case Datatype.UserDefined.Model(name) => {
        val privateName = GoUtil.privateName(name)
        Seq(
          s"$privateName, err := ${importBuilder.ensureImport("encoding/json")}.Marshal($varName)",
          "if (err != nil) {",
          "panic(err)".indentString(1),
          "}",
          "urlValues.Add(" + GoUtil.wrapInQuotes(param.name) + s", string($privateName))"
        ).mkString("\n")
      }

      case Datatype.UserDefined.Union(_) => {
        sys.error("TODO")
      }

      case Datatype.Container.Map(_) => {
        sys.error("TODO")
      }

    }
  }

  private[this] def build(
    keyName: String,
    varName: String,
    goType: GoType
  ): String = {
    "urlValues.Add(" + GoUtil.wrapInQuotes(keyName) + s", " + buildValue(varName, goType) + ")"
  }

  private[this] def buildValue(
    varName: String,
    goType: GoType
  ): String = {
    goType.datatype match {
      case _: Datatype.Primitive => {
        goType.toString(varName)
      }
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) => {
        sys.error("Cannot serialize model, union or map to parameter")
      }
      case Datatype.UserDefined.Enum(_) => {
        goType.toString(varName)
      }
      case Datatype.Container.Option(inner) => {
        buildValue(varName, GoType(importBuilder, inner))
      }
      case Datatype.Container.List(inner) => {
        buildValue(varName, GoType(importBuilder, inner))
      }
    }
  }

  private[this] def datatype(typeName: String, required: Boolean): Datatype = {
    datatypeResolver.parse(typeName, required).getOrElse {
      sys.error(s"Unknown datatype[$typeName]")
    }
  }


}
