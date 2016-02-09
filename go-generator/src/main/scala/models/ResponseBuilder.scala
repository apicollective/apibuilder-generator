package go.models

import Formatter._
import lib.{Datatype, DatatypeResolver}

case class ResponseBuilder(
  importBuilder: ImportBuilder,
  datatypeResolver: DatatypeResolver
) {

  def generate(resultsClassName: String, goType: GoType): String = {
    generate(resultsClassName, goType, goType.datatype)
  }

  private[this] def generate(resultsClassName: String, goType: GoType, datatype: Datatype): String = {
    val classVariableName = GoUtil.publicName(goType.classVariableName())
    val base = s"return $resultsClassName{StatusCode: resp.StatusCode, Response: resp%s}"
    val body = "resp.body"

    datatype match {
      case Datatype.Primitive.Unit => {
        base.format("")
      }
      case Datatype.Primitive.Boolean | Datatype.Primitive.Double | Datatype.Primitive.Integer | Datatype.Primitive.Long | Datatype.Primitive.DateIso8601 | Datatype.Primitive.DateTimeIso8601 | Datatype.Primitive.Decimal | Datatype.Primitive.String | Datatype.Primitive.Uuid => {
        base.format(s", $classVariableName: $body")
      }
      case Datatype.Primitive.Object => {
        val tmpVarName = GoUtil.privateName(classVariableName)
        val json = importBuilder.ensureImport("encoding/json")
        Seq(
          s"var $tmpVarName map[string]${goType.klass.localName}",
          s"${json}.NewDecoder(resp.Body).Decode(&$tmpVarName)",
          base.format(s", $classVariableName: $tmpVarName")
        ).mkString("\n")
      }
      case Datatype.UserDefined.Model(_) | Datatype.Container.Map(_) | Datatype.Container.List(_)=> {
        val tmpVarName = GoUtil.privateName(classVariableName)
        val json = importBuilder.ensureImport("encoding/json")
        Seq(
          s"var $tmpVarName ${goType.klass.localName}",
          s"${json}.NewDecoder(resp.Body).Decode(&$tmpVarName)",
          base.format(s", $classVariableName: $tmpVarName")
        ).mkString("\n")
      }
      case Datatype.UserDefined.Union(name) => {
        s"TODO UNION $name"
      }
      case Datatype.UserDefined.Enum(name) => {
        val method = GoUtil.publicName(name) + "FromString"
        base.format(s", $classVariableName: $name($body)")
      }
      case Datatype.Container.Option(inner) => {
        generate(resultsClassName, goType, inner)
      }
    }

    /*
        val tmpVarName = GoUtil.privateName(goType.classVariableName())
        val json = importBuilder.ensureImport("encoding/json")
        Seq(
          s"var $tmpVarName ${goType.klass.localName}",
          s"${json}.NewDecoder(resp.Body).Decode(&$tmpVarName)",
	  s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, $body: $tmpVarName}"
        ).mkString("\n")
      }
     */
  }

}
