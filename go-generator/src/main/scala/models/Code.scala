package go.models

import com.bryzek.apidoc.spec.v0.models.{Enum, Model, ParameterLocation, Resource, Service, Union}
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
        GoUtil.publicName(f.name) + " " + GoType(datatype(f.`type`, f.required)).className
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

  private[this] case class MethodArgumentsType(name: String)

  private[this] case class MethodResultsType(name: String)

  private[this] def generateResource(resource: Resource): String = {
    resource.operations.map { op =>
      val name = GeneratorUtil.urlToMethodName(resource.path, resource.operations.map(_.path), op.method, op.path)

      var methodParameters = scala.collection.mutable.ListBuffer[String]()
      methodParameters += "client Client"

      val argsType = op.parameters.filter(_.location == ParameterLocation.Query) match {
        case Nil => {
          None
        }
        case args => {
          val argsType = MethodArgumentsType(GoUtil.publicName("${name}Args"))
          methodParameters += s"args ${argsType.name}"
          Some(argsType)
        }
      }

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
      
      val resultsType = MethodResultsType(GoUtil.publicName(s"${name}Result"))



      Seq(
        s"func $name(${methodParameters.mkString(", ")}) ${resultsType.name} {",
        Seq(
          s"""requestUrl := fmt.Sprintf("%s$path", ${pathArgs.mkString(", ")})"""
        ).mkString("\n").indent(2),
        "}",
        ""
      ).mkString("\n")
    }.mkString("\n\n")
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
