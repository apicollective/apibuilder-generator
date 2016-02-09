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
              val paramGoType = GoType(importBuilder, datatype(p.`type`, p.required))
              val varName = s"${prefix}." + GoUtil.publicName(p.name)

              val declaration = p.default match {
                case None => {
                  build(varName, paramGoType)
                }
                case Some(default) => {
                  build(varName, paramGoType)
                }
              }

              "urlValues.Add(" + GoUtil.wrapInQuotes(p.name) + s", " + declaration + ")"
            }.mkString("\n")
          ).mkString("\n")
        )
      }
    }
  }

  private[this] def build(
    varName: String,
    goType: GoType
  ): String = {
    goType.datatype match {
      case v: Datatype.Primitive => goType.toString(varName)
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) => {
        sys.error("Cannot serialize model or union to parameter")
      }
      case Datatype.UserDefined.Enum(name) => {
        goType.toString(varName)
      }
      case Datatype.Container.Option(inner) => {
        build(varName, GoType(importBuilder, inner))
      }
      case Datatype.Container.List(inner) => {
        build(varName, GoType(importBuilder, inner))
      }
    }
  }

  private[this] def datatype(typeName: String, required: Boolean): Datatype = {
    datatypeResolver.parse(typeName, required).getOrElse {
      sys.error(s"Unknown datatype[$typeName]")
    }
  }


}
