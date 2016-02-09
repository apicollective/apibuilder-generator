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

  private[this] case class ResponseType(goType: GoType, name: String)

  private[this] val service = form.service
  private[this] val datatypeResolver = GeneratorUtil.datatypeResolver(service)
  private[this] val headers = Headers(form)
  private[this] val importBuilder = ImportBuilder()
  private[this] val urlValues = UrlValues(importBuilder, datatypeResolver)

  private[this] val hasClientBody: Boolean = {
    service.resources.map(_.operations).flatten.find { op =>
      !op.body.isEmpty || !op.parameters.find { _.location == ParameterLocation.Form }.isEmpty
    } match {
      case None => false
      case Some(_) => true
    }
  }

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
        val clientBody = generateClientBodyStruct()
        val footer = generateFooter()

        Some(
          Seq(
            Some(s"package ${GoUtil.packageName(service.name)}"),
            Some(ApidocComments(service.version: String, form.userAgent).comments.trim),
            Some(importBuilder.generate()),
            Some(headers.code),
            client,
            clientBody,
            Some(code),
            footer
          ).flatten.mkString("\n\n") + "\n"
        )
      }
    }
  }

  private[this] def generateEnum(enum: Enum): String = {
    val strings = importBuilder.ensureImport("strings")
    val enumName = GoUtil.publicName(enum.name)

    Seq(
      GoUtil.textToComment(enum.description) + s"type $enumName string",

      Seq(
        "const (",
        (
          enum.values.zipWithIndex.map { case (value, i) =>
            val comments = GoUtil.textToSingleLineComment(value.description)
            val name = enumName + GoUtil.publicName(value.name)
            val quotedValue = GoUtil.wrapInQuotes(value.name)

            i match {
              case 0 => comments + name + s" $enumName = $quotedValue"
              case _ => comments + name + s" = $quotedValue"
            }
          } ++ Seq(
            s"""${enumName}UNDEFINED = "UNDEFINED""""
          )
        ).mkString("\n").table().indent(1),
        ")"
      ).mkString("\n"),

      Seq(
        s"func ${enumName}FromString(value string) $enumName {",
        Seq(
          s"switch ${strings}.TrimSpace(${strings}.ToLower(value)) {",
          (
            enum.values.map { value =>
              val name = enumName + GoUtil.publicName(value.name)
              val key = GoUtil.wrapInQuotes(value.name.trim.toLowerCase)
              s"case $key:\n" + s"return $name".indent(1)
            } ++ Seq("default:\n" + s"return ${enumName}UNDEFINED".indent(1))
          ).mkString("\n"),
          "}"
        ).mkString("\n").table().indent(1),
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
          f.required match {
            case true => None
            case false => Some("omitempty")
          }
        ).flatten match {
          case Nil => None
          case els => Some(s"""`json:"${els.mkString(",")}"`""")
        }

        Seq(
          Some(publicName),
          Some(GoType(importBuilder, datatype(f.`type`, f.required)).klass.localName),
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
        GoUtil.publicName(typ.`type`) + " " + GoType(importBuilder, datatype(typ.`type`, true)).klass.localName
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

  private[this] case class MethodResponsesType(
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

      val resultsType = MethodResponsesType(
        name = GoUtil.publicName(s"${functionName}Response")
      )

      var methodParameters = mutable.ListBuffer[String]()
      methodParameters += "client Client"

      var queryParameters = mutable.ListBuffer[String]()

      var pathArgs = mutable.ListBuffer[String]()
      pathArgs += "client.BaseUrl"

      var path = op.path

      op.parameters.filter(_.location == ParameterLocation.Path).map { arg =>
        val varName = GoUtil.privateName(arg.name)
        val typ = GoType(importBuilder, datatype(arg.`type`, true))
        methodParameters += s"$varName ${typ.klass.localName}"
        path = path.replace(s":${arg.name}", "%s")
        pathArgs += typ.toEscapedString(varName)
      }

      val bodyString: Option[String] = op.body.map { body =>
        // TODO: val comments = GoUtil.textToComment(body.description)
        val varName = GoUtil.privateName(body.`type`)
        val typ = GoType(importBuilder, datatype(body.`type`, true))
        methodParameters += s"$varName ${typ.klass.localName}"
        "\n" + buildBody(varName, typ, resultsType)
      }
      
      val argsType: Option[MethodArgumentsType] = op.parameters.filter( p => p.location == ParameterLocation.Query || p.location == ParameterLocation.Form )  match {
        case Nil => {
          None
        }
        case params => {
          val argsType = MethodArgumentsType(
            name = GoUtil.publicName(s"${functionName}Params"),
            params = params
          )
          methodParameters += s"params ${argsType.name}"
          Some(argsType)
        }
      }

      val fmt = importBuilder.ensureImport("fmt")
      val errors = importBuilder.ensureImport("errors")

      var responseTypes = mutable.ListBuffer[ResponseType]()

      val queryString = urlValues.generate("params", op.parameters.filter(_.location == ParameterLocation.Query))
      val formString = urlValues.generate("params", op.parameters.filter(_.location == ParameterLocation.Form)).map { c =>
        val strings = importBuilder.ensureImport("strings")
        Seq(
          c,
         s"""body := ClientRequestBody{contentType: "application/x-www-form-urlencoded", bytes: ${strings}.NewReader(urlValues.Encode())}"""
        ).mkString("\n")
      }

      val queryToUrl = queryString match {
        case None => None
        case Some(_) => {
	  Some(
            Seq(
              "",
              "encodedValues := urlValues.Encode()",
              """if encodedValues != "" {""",
              s"""requestUrl += "?" + encodedValues""".indent(1),
              "}"
            ).mkString("\n")
          )
        }
      }

      val argsTypeCode = argsType.map { typ =>
        Seq(
          s"type ${typ.name} struct {",
          typ.params.map { param =>
            val goType = GoType(importBuilder, datatype(param.`type`, true))
            GoUtil.publicName(param.name) + " " + goType.klass.localName
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
                value >= 200 && value < 500 match {
                  case true => {
                    val goType = GoType(importBuilder, datatype(resp.`type`, true))
                    val varName = GoUtil.publicName(goType.classVariableName())
                    val responseType = ResponseType(goType, varName)
                    responseTypes += responseType

                    val responseExpr = goType.isUnit() match {
                      case true => {
                        s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp}"
                      }
                      case false => {
                        val tmpVarName = GoUtil.privateName(goType.classVariableName())
                        val json = importBuilder.ensureImport("encoding/json")
                        Seq(
                          s"var $tmpVarName ${goType.klass.localName}",
                          s"${json}.NewDecoder(resp.Body).Decode(&$tmpVarName)",
		          s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, $varName: $tmpVarName}"
                        ).mkString("\n")
                      }
                    }

                    Some(
                      Seq(
                        s"case $value:",
                        responseExpr.indent(1)
                      ).mkString("\n")
                    )
                  }

                  case false => {
                    Some(
                      Seq(
                        s"case $value:",
                        s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp}".indent(1)
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
                None // Intentional no-op. Let default case catch it
              }
            }
          } ++ Seq(
            Seq(
              "default:",
              s"return ${resultsType.name}{StatusCode: resp.StatusCode, Response: resp, Error: ${errors}.New(resp.Status)}".indent(1)
            ).mkString("\n")
          )
        ).mkString("\n\n"),
        "}"
      ).mkString("\n").indent(1)

      val responseTypeCode = if (responseTypes.isEmpty) {
        ""
      } else {
        val http = importBuilder.ensureImport("net/http")
        Seq(
          s"type ${resultsType.name} struct {",
          (
            Seq(
	      "StatusCode int",
  	      s"Response   *${http}.Response",
	      "Error      error"
            ) ++ responseTypes.filter(!_.goType.isUnit()).map { t => s"${t.name} ${t.goType.klass.localName}" }.distinct.sorted
          ).mkString("\n").table().indent(1),
          "}"
        ).mkString("\n") + "\n\n"
      }
      
      val bodyParam: String = hasClientBody match {
        case false => {
          ""
        }
        case true => {
          bodyString match {
            case None => {
              formString match {
                case None => ", ClientRequestBody{}"
                case Some(_) => ", body"
              }
            }
            case Some(_) => {
              s", body"
            }
          }
        }
      }

      Seq(
        s"${argsTypeCode}${responseTypeCode}func $functionName(${methodParameters.mkString(", ")}) ${resultsType.name} {",

        Seq(
          Some(s"""requestUrl := ${fmt}.Sprintf("%s$path", ${pathArgs.mkString(", ")})"""),
          bodyString,
          queryString,
          formString,
          queryToUrl
        ).flatten.mkString("\n").indent(1),

        Seq(
          s"""request, err := buildRequest(client, "${op.method}", requestUrl$bodyParam)""",
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

  private[this] def buildBody(varName: String, typ: GoType, responseType: MethodResponsesType): String = {
    val json = importBuilder.ensureImport("encoding/json")

    val bodyDefaults: Option[String] = typ.datatype match {
      case Datatype.UserDefined.Model(name) => {
        service.models.find(_.name == name) match {
          case None => {
            // TODO: How do we set defaults for imported models?
            println("WARNING: Could not find model named[$name]")
            None
          }
          case Some(m) => {
            m.fields.filter(!_.default.isEmpty).toList match {
              case Nil => {
                None
              }
              case fields => {
                Some(
                  fields.map { field =>
                    val fieldGoType = GoType(importBuilder, datatype(field.`type`, field.required))
                    val fieldName = GoUtil.publicName(field.name)
                    val fullName = s"${varName}.$fieldName"

                    Seq(
                      "if " + fieldGoType.nil(fullName) + " {",
                      s"$fullName = ${fieldGoType.declaration(field.default.get)}".indent(1),
                      "}"
                    ).mkString("\n")
                  }.mkString("\n\n").trim
                )
              }
            }
          }
        }
      }
      case _ => None
    }

    val bytes = importBuilder.ensureImport("bytes")

    Seq(
      bodyDefaults.map { code => s"\n$code\n" },
      Some(s"bodyDocument, err := ${json}.Marshal($varName)"),
      Some(
        Seq(
          "if err != nil {",
	  s"return ${responseType.name}{Error: err}".indent(1),
          "}"
        ).mkString("\n")
      ),
      Some(s"""body := ClientRequestBody{contentType: "application/json", bytes: ${bytes}.NewReader(bodyDocument)}""")
    ).flatten.mkString("\n")
  }

  private[this] def generateClientStruct(): Option[String] = {
    service.resources match {
      case Nil => {
        None
      }
      case _ => {
        val http = importBuilder.ensureImport("net/http")
        Some(s"""
type Client struct {
	HttpClient *${http}.Client
	Username   string
	Password   string
	BaseUrl    string
}
    """.trim)
      }
    }
  }

  def generateClientBodyStruct(): Option[String] = {
    hasClientBody match {
      case false => {
        None
      }
      case true => {
        val io = importBuilder.ensureImport("io")
        Some(s"""
type ClientRequestBody struct {
	contentType string
	bytes       ${io}.Reader
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
        val (bodyArg, bodyNewRequestArg) = hasClientBody match {
          case false => ("", "nil")
          case true => (", body ClientRequestBody", "body.bytes")
        }

        val http = importBuilder.ensureImport("net/http")
        Some(

          Seq(
            s"func buildRequest(client Client, method, urlStr string$bodyArg) (*${http}.Request, error) {",

            Seq(
              Some(
                Seq(
                  s"request, err := http.NewRequest(method, urlStr, $bodyNewRequestArg)",
	          "if err != nil {",
                  "return nil, err".indent(1),
                  "}"
                ).mkString("\n")
              ),

              Some(
                Seq(
                  "request.Header = map[string][]string{",
                  AllHeaders,
                  "}"
                ).mkString("\n")
              ),

              hasClientBody match {
                case false => None
                case true => Some(
	          Seq(
                    """if body.contentType != "" {""",
                    """request.Header["Content-type"] = []string{body.contentType}""".indent(1),
                    "}"
                  ).mkString("\n")
                )
              },

              Some(
                Seq(
                  """if client.Username != "" {""",
                  "request.SetBasicAuth(client.Username, client.Password)".indent(1),
                  "}"
                ).mkString("\n")
              ),

              Some("return request, nil")
            ).flatten.mkString("\n\n").indent(1),

            "}"

          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] val AllHeaders = headers.all.map {
    case (name, value) => s""""$name": {$value},""".indent(1)
  }.mkString("\n").table()

}
