package go.models

import com.bryzek.apidoc.spec.v0.models.{Enum, Model, Parameter, ParameterLocation, Resource, Service, Union}
import com.bryzek.apidoc.spec.v0.models.{ResponseCode, ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.Datatype
import lib.generator.GeneratorUtil
import lib.Text
import lib.Text._

case class Code(form: InvocationForm) {

  private[this] val service = form.service
  private[this] val datatypeResolver = GeneratorUtil.datatypeResolver(service)
  private[this] val headers = Headers(form)

  def generate(): Option[String] = {
    Seq(service.models, service.enums, service.unions).flatten.isEmpty match {
      case true => {
        None
      }
      case false => {
        Some(
          Seq(
            s"package ${GoUtil.packageName(service.name)}",
            BasicDefinitionTop,
            Seq(
              service.enums.map(generateEnum(_)),
              service.models.map(generateModel(_)),
              service.unions.map(generateUnion(_)),
              service.resources.map(generateResource(_))
            ).flatten.map(_.trim).filter(!_.isEmpty).mkString("\n\n"),
            BasicDefinitionBottom
          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] def generateEnum(enum: Enum): String = {
    Seq(
      s"type ${GoUtil.publicName(enum.name)} struct {",
      "TODO: Finish enum implementation",
      "}"
    ).mkString("\n")
  }

  private[this] def generateModel(model: Model): String = {
    Seq(
      s"type ${GoUtil.publicName(model.name)} struct {",
      model.fields.map { f =>
        val publicName = GoUtil.publicName(f.name)

        val json = Seq(
          (publicName == f.name) match {
            case true => None
            case fasle => Some(f.name)
          },
          !(f.required && f.default.isEmpty) match {
            case true => Some("omitempty")
            case false => None
          }
        ).flatten match {
          case Nil => None
          case els => Some(s"""`json:"${els.mkString(",")}"`""")
        }

        Seq(
          Some(publicName),
          Some(GoType(datatype(f.`type`, f.required)).className),
          json
        ).flatten.mkString("    ")
      }.mkString("\n").indent(2),
      "}\n"
    ).mkString("\n")
  }

  private[this] def generateUnion(union: Union): String = {
    Seq(
      s"type ${GoUtil.publicName(union.name)} struct {",
      union.types.map { typ =>
        GoUtil.publicName(typ.`type`) + " " + GoType(datatype(typ.`type`, true)).className
      }.mkString("\n").indent(2),
      "}\n"
    ).mkString("\n")
  }

  private[this] def datatype(typeName: String, required: Boolean): Datatype = {
    datatypeResolver.parse(typeName, required).getOrElse {
      sys.error(s"Unknown datatype[$typeName]")
    }
  }

  private[this] case class MethodArgumentsType(
    name: String,
    params: Seq[Parameter]
  ) {
    assert(!params.isEmpty, "Must have at least one parameter")
  }

  private[this] case class MethodResultsType(
    name: String
  )

  private[this] def generateResource(resource: Resource): String = {
    resource.operations.map { op =>
      val name = GeneratorUtil.urlToMethodName(resource.path, resource.operations.map(_.path), op.method, op.path)

      var methodParameters = scala.collection.mutable.ListBuffer[String]()
      methodParameters += "client Client"

      var queryParameters = scala.collection.mutable.ListBuffer[String]()

      var pathArgs = scala.collection.mutable.ListBuffer[String]()
      pathArgs += "client.baseUrl"

      var path = op.path

      op.parameters.filter(_.location == ParameterLocation.Path).map { arg =>
        val varName = GoUtil.privateName(arg.name)
        val typ = GoType(datatype(arg.`type`, true))
        methodParameters += s"$varName ${typ.className}"
        path = path.replace(s":${arg.name}", "%s")
        pathArgs += typ.toEscapedString(varName)
      }
      
      val argsType = op.parameters.filter(_.location == ParameterLocation.Query) match {
        case Nil => {
          None
        }
        case params => {
          val argsType = MethodArgumentsType(
            name = GoUtil.publicName(s"${name}Args"),
            params = params
          )
          methodParameters += s"args ${argsType.name}"
          Some(argsType)
        }
      }

      val resultsType = MethodResultsType(
        name = GoUtil.publicName(s"${name}Result")
      )

      var successType: Option[GoType] = None
      var successName: Option[String] = None

      val queryString = buildQueryString(op.parameters.filter(_.location == ParameterLocation.Query))
      val queryToUrl = queryString.map { _ =>
	Seq(
          "",
          "if len(params) > 0 {",
          """  requestUrl += fmt.Sprintf("?%s", strings.Join(params, "&"))""",
          "}"
        ).mkString("\n")
      }

      Seq(
        argsType.map { typ =>
          Seq(
            s"type ${typ.name} struct {",
            typ.params.map { param =>
              val goType = GoType(datatype(param.`type`, true))
              val varName = GoUtil.publicName(
                if (goType.isMulti) {
                  Text.pluralize(param.name)
                } else {
                  param.name
                }
              )

              varName + "    " + goType.className
            }.mkString("\n").indent(4),
            "}",
            ""
          ).mkString("\n")
        }.getOrElse(""),

        s"func $name(${methodParameters.mkString(", ")}) ${resultsType.name} {",

        Seq(
          Some(s"""requestUrl := fmt.Sprintf("%s$path", ${pathArgs.mkString(", ")})"""),
          queryString,
          queryToUrl
        ).flatten.mkString("\n").indent(2),

        "",
        Seq(
          s"""request, err := buildRequest(client, "${op.method}", requestUrl, nil)""",
          s"if err != nil {",
          s"  return ${resultsType.name}{Error: err}",
          s"}"
        ).mkString("\n").indent(2),

        "",
        Seq(
          s"resp, err := client.httpClient.Do(request)",
          s"if err != nil {",
          s"  return ${resultsType.name}{Error: err}",
          s"}",
          "defer resp.Body.Close()"
        ).mkString("\n").indent(2),

        "",
	Seq(
          "switch resp.StatusCode {",
          (
            op.responses.flatMap { resp =>
              resp.code match {
                case ResponseCodeInt(value) => {
                  value >= 200 && value < 300 match {
                    case true => {
                      val goType = GoType(datatype(resp.`type`, true))
                      successType = Some(goType)
                      successName = Some(GoUtil.publicName(goType.classVariableName()))
                      val varName = GoUtil.privateName(goType.classVariableName())

                      Some(
                        Seq(
                          s"case $value:",
                          s"var $varName ${goType.className}",
                          s"json.NewDecoder(resp.Body).Decode(&$varName)",
		          s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, ${successName.get}: $varName}"
                        ).mkString("\n")
                      )
                    }

                    case false => {
                      // TODO: Handle error types here
                      Some(
                        Seq(
                          s"case $value:",
                          s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, Error: errors.New(resp.Status)}"
                        ).mkString("\n")
                      )
                    }
                  }
                }
                case ResponseCodeOption.Default => {
                  Some(
                    Seq(
                      s"case resp.StatusCode >= 200 && resp.StatusCode < 300:",
                      s"// TODO"
                    ).mkString("\n")
                  )
                }
                case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
                  None // No-op. Let default case catch it
                }
              }
            } ++ Seq(
              Seq(
                "case default:",
                s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, Error: errors.New(resp.Status)}"
              ).mkString("\n")
            )
          ).mkString("\n\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}",

        "",
        successName.map { name =>
          Seq(
            s"type ${resultsType.name} struct {",
            Seq(
	      "StatusCode   int",
	      "Response     *http.Response",
	      "Error        error",
	      s"$name         ${successType.get.className}"
            ).mkString("\n").indent(2),
            "}"
          ).mkString("\n")
        }.mkString("\n")
      ).mkString("\n")
    }.mkString("\n\n")
  }

  private[this] def buildQueryString(params: Seq[Parameter]): Option[String] = {
    params match {
      case Nil => None
      case _ => Some(
        "\nparams := []string{}\n\n" + params.map { p => buildQueryString(p, datatype(p.`type`, true))}.mkString("\n\n")
      )
    }
  }

  private[this] def buildQueryString(param: Parameter, datatype: Datatype): String = {
    val fieldName = "args." + GoUtil.publicName(param.name)
    val goType = GoType(datatype)

    datatype match {
      case t: Datatype.Primitive => {
        param.default match {
          case None => {
            Seq(
              "if " + goType.notNil(fieldName) + " {",
              "  " + addSingleParam(param.name, datatype, fieldName),
              "}"
            ).mkString("\n")
          }
          case Some(default) => {
            Seq(
              "if " + goType.nil(fieldName) + " {",
              "  " + addSingleParam(param.name, datatype, GoUtil.wrapInQuotes(default)),
              "} else {",
              "  " + addSingleParam(param.name, datatype, fieldName),
              "}"
            ).mkString("\n")

          }
        }
      }
      case Datatype.Container.List(inner) => {
        Seq(
          s"for _, value := range $fieldName {",
          "  " + addSingleParam(param.name, inner, "value"),
          "}"
        ).mkString("\n")
      }
      case Datatype.Container.Option(inner) => {
        buildQueryString(param, inner)
      }
      case Datatype.UserDefined.Enum(name) => {
        buildQueryString(param, Datatype.Primitive.String)
      }
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) | Datatype.Container.Map(_) => {
        sys.error(s"Parameter $param cannot be converted to query string")
      }
    }
  }

  private[this] def addSingleParam(name: String, datatype: Datatype, varName: String): String = {
    s"""params = append(params, fmt.Sprintf("$name=%s", """ + toQuery(datatype, varName) + "))"
  }

  private[this] def toQuery(datatype: Datatype, varName: String): String = {
    val expr = GoType(datatype).toString(varName)
    expr == varName match {
      case true => s"url.QueryEscape($varName)"
      case false => expr
    }
  }

  private[this] val AllHeaders = headers.all.map {
    case (name, value) => s"""		"$name":   {$value},"""
  }.mkString("\n")

  private[this] val BasicDefinitionTop = s"""
import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/flowcommerce/tools/profile"
	"io"
	"net/html"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"sync"
)

${headers.code}

type Client struct {
	httpClient                  *http.Client
	username                    string
	password                    string
	baseUrl                     string
}
    """.trim

  private[this] val BasicDefinitionBottom = s"""
func buildRequest(client Client, method, urlStr string, body io.Reader) (*http.Request, error) {
	request, err := http.NewRequest(method, urlStr, body)
	if err != nil {
		return nil, err
	}

	request.Header = map[string][]string{
$AllHeaders
	}

	if client.username != "" {
		request.SetBasicAuth(client.username, client.password)
	}

	return request, nil
}
  """.trim

}
