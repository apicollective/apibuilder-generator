package go.models

import com.bryzek.apidoc.spec.v0.models.{Enum, Model, Parameter, ParameterLocation, Resource, Service, Union}
import com.bryzek.apidoc.spec.v0.models.{ResponseCode, ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import Formatter._
import lib.Datatype
import lib.generator.GeneratorUtil
import lib.Text
import scala.collection.mutable


case class Code(form: InvocationForm) {

  private[this] val service = form.service
  private[this] val datatypeResolver = GeneratorUtil.datatypeResolver(service)
  private[this] val headers = Headers(form)
  private[this] val importBuilder = ImportBuilder()

  def generate(): Option[String] = {
    Seq(service.models, service.enums, service.unions).flatten.isEmpty match {
      case true => {
        None
      }
      case false => {
        val code = Seq(
          service.enums.map(generateEnum(_)),
          service.models.map(generateModel(_)),
          service.unions.map(generateUnion(_)),
          service.resources.map(generateResource(_))
        ).flatten.map(_.trim).filter(!_.isEmpty).mkString("\n\n")

        // Generate all code here as we need to process imports
        val client = generateClientStruct()
        val footer = generateFooter()

        Some(
          Seq(
            Some(s"package ${GoUtil.packageName(service.name)}"),
            Some(ApidocComments(service.version: String, form.userAgent).comments.trim),
            Some(importBuilder.generate()),
            Some(headers.code),
            client,
            Some(code),
            footer
          ).flatten.mkString("\n\n") + "\n"
        )
      }
    }
  }

  private[this] def generateEnum(enum: Enum): String = {
    importBuilder.ensureImport("fmt")
    val enumName = GoUtil.publicName(enum.name)

    Seq(
      GoUtil.textToComment(enum.description) + s"type $enumName int",

      Seq(
        "const (",
        enum.values.zipWithIndex.map { case (value, i) =>
          val text = GoUtil.textToSingleLineComment(value.description) + GoUtil.publicName(value.name)
          i match {
            case 0 => text + s" $enumName = iota"
            case _ => text
          }
        }.mkString("\n").indent(1),
        ")"
      ).mkString("\n"),

      Seq(
        s"func (value $enumName) String() string {",
        Seq(
          "switch value {",
          (
            enum.values.zipWithIndex.map { case (value, i) =>
              s"case $i:\n" + "return ".indent(1) + GoUtil.wrapInQuotes(value.name)
            } ++ Seq("default:\n" + "return fmt.Sprintf(".indent(1) + GoUtil.wrapInQuotes(s"$enumName[%v]") + ", value)")
          ).mkString("\n"),
          "}"
        ).mkString("\n").indent(1),
        "}"
      ).mkString("\n")

    ).mkString("\n\n")
  }

  private[this] def generateModel(model: Model): String = {
    Seq(
      GoUtil.textToComment(model.description) + s"type ${GoUtil.publicName(model.name)} struct {",
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
          Some(GoType(importBuilder, datatype(f.`type`, f.required)).className),
          json
        ).flatten.mkString(" ")
      }.mkString("\n").table().indent(1),
      "}\n"
    ).mkString("\n")
  }

  private[this] def generateUnion(union: Union): String = {
    Seq(
      GoUtil.textToComment(union.description) + s"type ${GoUtil.publicName(union.name)} struct {",
      union.types.map { typ =>
        GoUtil.publicName(typ.`type`) + " " + GoType(importBuilder, datatype(typ.`type`, true)).className
      }.mkString("\n").table().indent(1),
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
      val functionName = Seq(
        GoUtil.publicName(resource.plural),
        GoUtil.publicName(
          GeneratorUtil.urlToMethodName(resource.path, resource.operations.map(_.path), op.method, op.path)
        )
      ).mkString("")

      var methodParameters = mutable.ListBuffer[String]()
      methodParameters += "client Client"

      var queryParameters = mutable.ListBuffer[String]()

      var pathArgs = mutable.ListBuffer[String]()
      pathArgs += "client.BaseUrl"

      var path = op.path

      op.parameters.filter(_.location == ParameterLocation.Path).map { arg =>
        val varName = GoUtil.privateName(arg.name)
        val typ = GoType(importBuilder, datatype(arg.`type`, true))
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
            name = GoUtil.publicName(s"${functionName}Args"),
            params = params
          )
          methodParameters += s"args ${argsType.name}"
          Some(argsType)
        }
      }

      val resultsType = MethodResultsType(
        name = GoUtil.publicName(s"${functionName}Result")
      )

      var successType: Option[GoType] = None
      var successName: Option[String] = None

      val queryString = buildQueryString(op.parameters.filter(_.location == ParameterLocation.Query))
      val queryToUrl = queryString.map { _ =>
        importBuilder.ensureImport("strings")
        importBuilder.ensureImport("fmt")
	Seq(
          "",
          "if len(params) > 0 {",
          """requestUrl += fmt.Sprintf("?%s", strings.Join(params, "&"))""".indent(1),
          "}"
        ).mkString("\n")
      }

      importBuilder.ensureImport("fmt")
      importBuilder.ensureImport("errors")

      val argsTypeCode = argsType.map { typ =>
        Seq(
          s"type ${typ.name} struct {",
          typ.params.map { param =>
            val goType = GoType(importBuilder, datatype(param.`type`, true))
            GoUtil.publicName(param.name) + " " + goType.className
          }.mkString("\n").table().indent(1),
          "}",
          ""
        ).mkString("\n")
      } match {
        case None => ""
        case Some(code) => s"$code\n"
      }

      val processResponses = Seq(
        "switch resp.StatusCode {",
        (
          op.responses.flatMap { resp =>
            resp.code match {
              case ResponseCodeInt(value) => {
                value >= 200 && value < 300 match {
                  case true => {
                    val goType = GoType(importBuilder, datatype(resp.`type`, true))
                    successType = Some(goType)
                    successName = Some(GoUtil.publicName(goType.classVariableName()))
                    val varName = GoUtil.privateName(goType.classVariableName())

                    importBuilder.ensureImport("encoding/json")

                    Some(
                      Seq(
                        s"case $value:",
                        Seq(
                          s"var $varName ${goType.className}",
                          s"json.NewDecoder(resp.Body).Decode(&$varName)",
		          s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, ${successName.get}: $varName}"
                        ).mkString("\n").indent(1)
                      ).mkString("\n")
                    )
                  }

                  case false => {
                    importBuilder.ensureImport("errors")
                    // TODO: Handle error types here
                    Some(
                      Seq(
                        s"case $value:",
                        s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, Error: errors.New(resp.Status)}".indent(1)
                      ).mkString("\n")
                    )
                  }
                }
              }
              case ResponseCodeOption.Default => {
                Some(
                  Seq(
                    s"case resp.StatusCode >= 200 && resp.StatusCode < 300:",
                    s"// TODO".indent(1)
                  ).mkString("\n")
                )
              }
              case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
                None // No-op. Let default case catch it
              }
            }
          } ++ Seq(
            Seq(
              "default:",
              s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, Error: errors.New(resp.Status)}".indent(1)
            ).mkString("\n")
          )
        ).mkString("\n\n"),
        "}"
      ).mkString("\n").indent(1)

      val responseTypeCode = successName.map { name =>
        importBuilder.ensureImport("net/http")
        Seq(
          s"type ${resultsType.name} struct {",
          Seq(
	    "StatusCode int",
	    "Response   *http.Response",
	    "Error      error",
	    s"$name     ${successType.get.className}"
          ).mkString("\n").table().indent(1),
          "}"
        ).mkString("\n")
      } match {
        case None => ""
        case Some(code) => s"$code\n\n"
      }
      
      Seq(
        s"${argsTypeCode}${responseTypeCode}func $functionName(${methodParameters.mkString(", ")}) ${resultsType.name} {",

        Seq(
          Some(s"""requestUrl := fmt.Sprintf("%s$path", ${pathArgs.mkString(", ")})"""),
          queryString,
          queryToUrl
        ).flatten.mkString("\n").indent(1),

        Seq(
          s"""request, err := buildRequest(client, "${op.method}", requestUrl, nil)""",
          s"if err != nil {",
          s"return ${resultsType.name}{Error: err}".indent(1),
          s"}"
        ).mkString("\n").indent(1),

        Seq(
          s"resp, err := client.HttpClient.Do(request)",
          s"if err != nil {",
          s"return ${resultsType.name}{Error: err}".indent(1),
          s"}",
          "defer resp.Body.Close()"
        ).mkString("\n").indent(1),

        processResponses,

        "}"

      ).mkString("\n\n")
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
    val goType = GoType(importBuilder, datatype)

    datatype match {
      case t: Datatype.Primitive => {
        param.default match {
          case None => {
            Seq(
              "if " + goType.notNil(fieldName) + " {",
              addSingleParam(param.name, datatype, fieldName).indent(1),
              "}"
            ).mkString("\n")
          }
          case Some(default) => {
            Seq(
              "if " + goType.nil(fieldName) + " {",
              addSingleParam(param.name, datatype, GoUtil.wrapInQuotes(default)).indent(1),
              "} else {",
              addSingleParam(param.name, datatype, fieldName).indent(1),
              "}"
            ).mkString("\n")

          }
        }
      }
      case Datatype.Container.List(inner) => {
        Seq(
          s"for _, value := range $fieldName {",
          addSingleParam(param.name, inner, "value").indent(1),
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
    importBuilder.ensureImport("fmt")
    s"""params = append(params, fmt.Sprintf("$name=%s", """ + toQuery(datatype, varName) + "))"
  }

  private[this] def toQuery(datatype: Datatype, varName: String): String = {
    val expr = GoType(importBuilder, datatype).toString(varName)
    expr == varName match {
      case true => {
        importBuilder.ensureImport("net/url")
        s"url.QueryEscape($varName)"
      }
      case false => expr
    }
  }

  private[this] val AllHeaders = headers.all.map {
    case (name, value) => s""""$name": {$value},""".indent(1)
  }.mkString("\n")


  // other: "bytes", "sync"
  private[this] def generateClientStruct(): Option[String] = {
    service.resources match {
      case Nil => {
        None
      }
      case _ => {
        importBuilder.ensureImport("net/http")
        Some("""
type Client struct {
	HttpClient *http.Client
	Username   string
	Password   string
	BaseUrl    string
}
    """.trim)
      }
    }
  }

  private[this] def generateFooter(): Option[String] = {
    service.resources match {
      case Nil => {
        None
      }
      case _ => {
        importBuilder.ensureImport("io")
        importBuilder.ensureImport("net/http")
        Some(s"""
func buildRequest(client Client, method, urlStr string, body io.Reader) (*http.Request, error) {
	request, err := http.NewRequest(method, urlStr, body)
	if err != nil {
		return nil, err
	}

	request.Header = map[string][]string{
${AllHeaders.indent(1).table()}
	}

	if client.Username != "" {
		request.SetBasicAuth(client.Username, client.Password)
	}

	return request, nil
}
  """.trim)
      }
    }
  }
}
