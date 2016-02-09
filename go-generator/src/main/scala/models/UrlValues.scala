package go.models

import Formatter._
import com.bryzek.apidoc.spec.v0.models.Parameter
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
      case t: Datatype.Primitive => {
        param.default match {
          case None => {
            Seq(
              "if " + goType.notNil(varName) + " {",
              build(param.name, varName, goType).indent(1),
              "}"
            ).mkString("\n")
          }
          case Some(default) => {
            Seq(
              "if " + goType.nil(varName) + " {",
              GoType.isNumeric(datatype) match {
                case true => {
                  build(param.name, default, goType).indent(1)
                }
                case false => {
                  build(param.name, GoUtil.wrapInQuotes(default), goType).indent(1)
                }
              },
              "} else {",
              build(param.name, varName, goType).indent(1),
              "}"
            ).mkString("\n")
          }
        }
      }
      case Datatype.Container.List(inner) => {
        Seq(
          s"for _, value := range $varName {",
          build(param.name, "value", goType).indent(1),
          "}"
        ).mkString("\n")
      }
      case Datatype.Container.Option(inner) => {
        buildParam(prefix, param, inner)
      }
      case Datatype.UserDefined.Enum(name) => {
        sys.error("Enums not yet supported")
      }
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) => {
        sys.error(s"Parameter $param cannot be converted to query string")
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
      case v: Datatype.Primitive => {
        goType.toString(varName)
      }
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) => {
        sys.error("Cannot serialize model or union to parameter")
      }
      case Datatype.UserDefined.Enum(name) => {
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
