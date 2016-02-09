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
        Some(
          Seq(
	    "urlValues := url.Values{}",
            params.map { p =>
              val goType = GoType(importBuilder, datatype(p.`type`, p.required))
              val varName = s"${prefix}." + GoUtil.publicName(p.name)

              p.default match {
                case None => {
                  Seq(
                    "if " + goType.notNil(varName) + " {",
                    build(p.name, varName, goType).indent(1),
                    "}"
                  ).mkString("\n")
                }
                case Some(default) => {
                  Seq(
                    "if " + goType.notNil(varName) + " {",
                    build(p.name, varName, goType).indent(1),
                    "} else {",
                    ("urlValues.Add(" + GoUtil.wrapInQuotes(p.name) + s", " + goType.toString(default) + ")").indent(1),
                    "}"
                  ).mkString("\n")
                }
              }
            }.mkString("\n")
          ).mkString("\n")
        )
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
