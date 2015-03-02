package models

import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models._
import lib.{Datatype, Kind, Methods, Primitives, Text, Type, VersionTag}
import lib.Text._
import generator.{GeneratorUtil, CodeGenerator, ScalaUtil}
import scala.collection.mutable.ListBuffer
import play.api.Logger

object RubyUtil {

  case class Module(namespace: String) {
    val parts = namespace.split("\\.").map(toClassName(_))
    val fullName = "::" + parts.mkString("::")
  }

  def textToComment(text: Seq[String]): String = {
    text.map(s => s"# $s").mkString("\n")
  }

  def textToComment(text: String): String = {
    if (text.trim.isEmpty) {
      ""
    } else {
      textToComment(GeneratorUtil.splitIntoLines(text))
    }
  }

  def toClassName(
    name: String,
    multiple: Boolean = false
  ): String = {
    ScalaUtil.toClassName(name, multiple)
  }

  def toConstant(
    name: String
  ): String = {
    Text.splitIntoWords(Text.camelCaseToUnderscore(name)).map(_.toUpperCase).mkString("_")
  }

  /**
    * Since ruby does not give us multiple inheritance, we create a
    * wrapper for each combination of union type / primitive to avoid
    * conflicts if the same primitive type is used in more than one
    * union type.
    */
  def toUnionConstant(
    union: Union,
    name: String
  ): String = {
    Primitives(name) match {
      case None => RubyUtil.toConstant(name)
      case Some(p) => RubyUtil.toConstant(RubyPrimitiveWrapper.className(union, p))
    }
  }

  def toDefaultVariable(
    multiple: Boolean = false
  ): String = {
    toVariable("value", multiple = multiple)
  }

  def toVariable(datatype: Datatype): String = {

    val multiple = Container(datatype).multiple
    datatype.`type` match {
      case Type(Kind.Primitive, _) => RubyUtil.toDefaultVariable(multiple = multiple)
      case Type(Kind.Model | Kind.Enum | Kind.Union, name) => RubyUtil.toVariable(name, multiple = multiple)
    }
  }

  def toVariable(
    name: String,
    multiple: Boolean = false
  ): String = {
    val value = lib.Text.initLowerCase(lib.Text.camelCaseToUnderscore(name).toLowerCase)
    multiple match {
      case true => lib.Text.pluralize(value)
      case false => value
    }
  }

  def wrapInQuotes(value: String): String = {
    if (value.indexOf("'") < 0) {
      s"'$value'"
    } else if (value.indexOf("\"") < 0) {
      s""""$value""""
    } else {
      sys.error("TODO: Support quoting quotes")
    }
  }

}

object RubyClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): String = {
    new RubyClientGenerator(form).invoke
  }

  def classDeclaration(name: String, parent: Option[String]): String = {
    Seq(
      Some(s"class $name"),
      parent.map { p => s"< $p" }
    ).flatten.mkString(" ")
  }

  def generateEnum(enum: Enum, union: Option[Union]): String = {
    val className = RubyUtil.toClassName(enum.name)

    val lines = ListBuffer[String]()
    lines.append(classDeclaration(className, union.map { u => RubyUtil.toClassName(u.name) }))

    lines.append("")
    lines.append("  attr_reader :value")

    lines.append("")
    lines.append("  def initialize(value)")
    union.map { u =>
      lines.append(s"    super(:name => ${RubyUtil.toClassName(u.name)}::Types::${RubyUtil.toUnionConstant(u, enum.name)})")
    }

    lines.append("    @value = HttpClient::Preconditions.assert_class('value', value, String)")
    lines.append("  end")

    lines.append("")
    lines.append(s"  # Returns the instance of ${className} for this value, creating a new instance for an unknown value")
    lines.append(s"  def $className.apply(value)")
    lines.append(s"    if value.instance_of?($className)")
    lines.append(s"      value")
    lines.append(s"    else")
    lines.append(s"      HttpClient::Preconditions.assert_class_or_nil('value', value, String)")
    lines.append(s"      value.nil? ? nil : (from_string(value) || $className.new(value))")
    lines.append(s"    end")
    lines.append(s"  end")
    lines.append("")
    lines.append(s"  # Returns the instance of $className for this value, or nil if not found")
    lines.append(s"  def $className.from_string(value)")
    lines.append("    HttpClient::Preconditions.assert_class('value', value, String)")
    lines.append(s"    $className.ALL.find { |v| v.value == value }")
    lines.append("  end")

    lines.append("")
    lines.append(s"  def $className.ALL") // Upper case to avoid naming conflict
    lines.append("    @@all ||= [" + enum.values.map(v => s"$className.${enumName(v.name)}").mkString(", ") + "]")
    lines.append("  end")

    lines.append("")
    enum.values.foreach { value =>
      val varName = enumName(value.name)
      value.description.foreach { desc =>
        lines.append(GeneratorUtil.formatComment(desc).indent(2))
      }
      lines.append(s"  def $className.$varName")
      lines.append(s"    @@_$varName ||= $className.new('${value.name}')")
      lines.append(s"  end")
      lines.append("")
    }

    val toHashMethodName = union match {
      case None => "to_hash"
      case Some(_) => "subtype_to_hash"
    }

    lines.append(s"  def $toHashMethodName")
    lines.append("    value")
    lines.append("  end")
    lines.append("")

    lines.append("end")
    lines.mkString("\n")
  }

  def enumName(value: String): String = {
    val formatted = if (value == value.toUpperCase) {
      value.toLowerCase
    } else {
      lib.Text.camelCaseToUnderscore(value)
    }

    Text.splitIntoWords(formatted).map(lib.Text.initLowerCase(_)).mkString("_")
  }

}

/**
 * Generates a Ruby Client file based on the service description
 * from api.json
 */
case class RubyClientGenerator(form: InvocationForm) {
  private val service = form.service

  private val module = RubyUtil.Module(service.namespace)

  private val datatypeResolver = GeneratorUtil.datatypeResolver(service)

  private val primitiveWrapper = RubyPrimitiveWrapper(service)

  /**
    * Ruby does not support multiple inheritance. This method
    * standardizes how we select the single union type.
    */
  private def singleUnion(unions: Seq[Union]): Option[Union] = {
    unions.toList match {
      case Nil => None
      case single :: Nil => Some(single)
      case multiple => {
        val types = multiple.map(_.name).mkString(", ")
        Logger.warn("Ruby client does not support multiple inheritance. Multiple union types: ${types}. Using first type")
        Some(multiple.head)
      }
    }
  }

  private def unionsFor(model: Model): Seq[Union] = {
    service.unions.filter { u =>
      u.types.map(_.`type`).contains(model.name)
    }
  }

  private def unionsFor(enum: Enum): Seq[Union] = {
    service.unions.filter { u =>
      u.types.map(_.`type`).contains(enum.name)
    }
  }

  def invoke(): String = {
    val spacerSize = 2
    val moduleIndent = spacerSize * module.parts.size

    Seq(
      ApidocComments(form.service.version, form.userAgent).toRubyString(),
      RubyHttpClient.require,
      Seq(
        service.description.map { desc => GeneratorUtil.formatComment(desc) + "\n" }.getOrElse("") +
          module.parts.zipWithIndex.map { case (name, i) => s"module $name".indent(spacerSize * i) }.mkString("\n"),
        generateClient().indent(moduleIndent),
        "",
        Seq(
          "module Clients",
          service.resources.map { generateClientForResource(_) }.mkString("\n\n").indent(2),
          "end"
        ).mkString("\n\n").indent(moduleIndent),
        "",
        Seq(
          "module Models",
          Seq(
            service.unions.map { generateUnion(_) },
            service.enums.map { e => RubyClientGenerator.generateEnum(e, singleUnion(unionsFor(e))) },
            service.models.map { m => generateModel(m, singleUnion(unionsFor(m))) },
            primitiveWrapper.wrappers.map { w => generateModel(w.model, Some(w.union)) }
          ).filter(!_.isEmpty).flatten.mkString("\n\n").indent(2),
          "end"
        ).mkString("\n\n").indent(moduleIndent),
        "",
        "# ===== END OF SERVICE DEFINITION =====".indent(moduleIndent),
        RubyHttpClient.contents.indent(moduleIndent),
        module.parts.zipWithIndex.reverse.map { case (name, i) => "end".indent(spacerSize * i) }.mkString("\n")
      ).mkString("\n")
    ).mkString("\n\n")
  }

  private def generateClient(): String = {
    val sb = ListBuffer[String]()
    val url = service.baseUrl

    val headers = Headers(form)

    val headerString = headers.ruby.map { case (name, value) =>
      s"with_header(${RubyUtil.wrapInQuotes(name)}, $value)"
    }.mkString(".", ".", "")

    sb.append(s"""
class Client

${headers.rubyModuleConstants.indent(2)}

  def initialize(url, opts={})
    @url = HttpClient::Preconditions.assert_class('url', url, String)
    @authorization = HttpClient::Preconditions.assert_class_or_nil('authorization', opts.delete(:authorization), HttpClient::Authorization)
    HttpClient::Preconditions.assert_empty_opts(opts)
    HttpClient::Preconditions.check_state(url.match(/http.+/i), "URL[%s] must start with http" % url)
  end

  def request(path=nil)
    HttpClient::Preconditions.assert_class_or_nil('path', path, String)
    request = HttpClient::Request.new(URI.parse(@url + path.to_s))$headerString

    if @authorization
      request.with_auth(@authorization)
    else
      request
    end
  end
""")

    sb.append(service.resources.map { resource =>
      val className = RubyUtil.toClassName(resource.plural)

      s"  def ${resource.plural}\n" +
      s"    @${resource.plural} ||= ${module.fullName}::Clients::${className}.new(self)\n" +
      s"  end"
    }.mkString("\n\n"))

    sb.append("end")

    sb.mkString("\n")
  }

  def generateClientForResource(resource: Resource): String = {
    val className = RubyUtil.toClassName(resource.plural)

    val sb = ListBuffer[String]()
    sb.append(s"class ${className}")
    sb.append("")
    sb.append("  def initialize(client)")
    sb.append(s"    @client = HttpClient::Preconditions.assert_class('client', client, ${module.fullName}::Client)")
    sb.append("  end")

    resource.operations.foreach { op =>
      val pathParams = op.parameters.filter { p => p.location == ParameterLocation.Path }
      val queryParams = op.parameters.filter { p => p.location == ParameterLocation.Query }
      val formParams = op.parameters.filter { p => p.location == ParameterLocation.Form }

      val rubyPath = op.path.split("/").map { name =>
        if (name.startsWith(":")) {
          val varName = name.slice(1, name.length)
          val param = pathParams.find(_.name == varName).getOrElse {
            sys.error(s"Could not find path parameter named[$varName]")
          }
          val dt = datatypeResolver.parse(param.`type`).getOrElse {
            sys.error(s"Invalid type[${param.`type`}]")
          }
          dt match {
            case Datatype.Singleton(_) => {
              dt.`type` match {
                case Type(Kind.Primitive, name) => {
                  val code = asString(RubyUtil.toVariable(varName), name, escape = true)
                  s"#{$code}"
                }
                case Type(Kind.Model, name) => {
                  sys.error("Models cannot be in the path")
                }
                case Type(Kind.Union, name) => {
                  sys.error("Unions cannot be in the path")
                }
                case Type(Kind.Enum, name) => {
                  val code = RubyUtil.toVariable(varName)
                  s"#{$code.value}"
                }
              }
            }
            case Datatype.List(_) => {
              sys.error("Cannot have lists in the path")
            }
            case Datatype.Map(_) => {
              sys.error("Cannot have maps in the path")
            }
          }
        } else {
          name
        }
      }.mkString("/")

      val methodName =lib.Text.camelCaseToUnderscore(
        GeneratorUtil.urlToMethodName(
          resource.plural,
          op.method,
          op.path
        )
      ).toLowerCase

      val paramStrings = ListBuffer[String]()
      pathParams.map(_.name).foreach { n => paramStrings.append(RubyUtil.toVariable(n)) }

      if (Methods.isJsonDocumentMethod(op.method.toString)) {
        op.body.flatMap(body => datatypeResolver.parse(body.`type`)) match {
          case None => paramStrings.append("hash")
          case Some(dt) => {
            val multiple = dt match {
              case Datatype.Singleton(_) => false
              case Datatype.List(_) | Datatype.Map(_) => true
            }

            paramStrings.append(RubyUtil.toVariable(dt))
          }
        }
      }

      if (!queryParams.isEmpty) {
        paramStrings.append("incoming={}")
      }

      sb.append("")
      op.description.map { desc =>
        sb.append(GeneratorUtil.formatComment(desc, 2))
      }

      val paramCall = if (paramStrings.isEmpty) { "" } else { "(" + paramStrings.mkString(", ") + ")" }
      sb.append(s"  def ${methodName}$paramCall")

      pathParams.foreach { param =>
        val ti = parseType(param.`type`, fieldName = Some(param.name))
        sb.append("    " + ti.assertMethod)
      }

      if (!queryParams.isEmpty) {
        val paramBuilder = ListBuffer[String]()

        queryParams.foreach { param =>
          paramBuilder.append(s":${param.name} => ${parseArgument(param.name, param.`type`, param.required, param.default)}")
        }

        sb.append("    opts = HttpClient::Helper.symbolize_keys(incoming)")
        sb.append("    query = {")
        sb.append("      " + paramBuilder.mkString(",\n      "))
        sb.append("    }.delete_if { |k, v| v.nil? }")
      }

      val requestBuilder = new StringBuilder()
      requestBuilder.append("@client.request(\"" + rubyPath + "\")")

      if (!queryParams.isEmpty) {
        requestBuilder.append(".with_query(query)")
      }

      if (Methods.isJsonDocumentMethod(op.method.toString)) {
        op.body.map(_.`type`) match {
          case None => {
            sb.append("    HttpClient::Preconditions.assert_class('hash', hash, Hash)")
            requestBuilder.append(".with_json(hash.to_json)")
          }
          case Some(body) => {
            val ti = parseType(body)
            sb.append("    " + ti.assertMethod)

            ti.datatype.`type` match {
              case Type(Kind.Primitive, name) => {
                Container(ti.datatype) match {
                  case Container.Singleton => {
                    requestBuilder.append(s".with_body(${ti.varName})")
                  }
                  case Container.List => {
                    requestBuilder.append(s".with_json(${ti.varName}")
                  }
                  case Container.Map => {
                    requestBuilder.append(s".with_json(${ti.varName}")
                  }
                }
              }

              case Type(Kind.Model | Kind.Enum | Kind.Union, _) => {
                Container(ti.datatype) match {
                  case Container.Singleton => {
                    requestBuilder.append(s".with_json(${ti.varName}.to_json)")
                  }
                  case Container.List => {
                    requestBuilder.append(s".with_json(${ti.varName}.map { |o| o.to_hash }.to_json)")
                  }
                  case Container.Map => {
                    requestBuilder.append(s".with_json(${ti.varName}.inject({}) { |hash, o| hash[o[0]] = o[1].nil? ? nil : o[1].to_hash; hash }).to_json")
                  }
                }
              }

            }
          }
          case _ => sys.error(s"Invalid body [${op.body}]")
        }
      }
      requestBuilder.append(s".${op.method.toString.toLowerCase}")

      val response = generateResponses(op)

      sb.append(s"    ${requestBuilder.toString}$response")
      sb.append("  end")
    }

    sb.append("")
    sb.append("end")

    sb.mkString("\n")
  }

  private def undefinedTypeClassName(union: Union): String = {
    RubyUtil.toClassName(union.name + "UndefinedType")
  }

  def generateUnion(union: Union): String = {
    Seq(
      generateUnionClass(union),
      generateUnionUndefinedType(union)
    ).mkString("\n\n")
  }

  def generateUnionClass(union: Union): String = {
    val className = RubyUtil.toClassName(union.name)
    union.description.map { desc => GeneratorUtil.formatComment(desc) + "\n" }.getOrElse("") + s"class $className\n\n" +
    Seq(
      Seq(
        "module Types",
        union.types.map { ut =>
          ut.description.map { desc => GeneratorUtil.formatComment(desc) }.getOrElse("") +
          s"${RubyUtil.toUnionConstant(union, ut.`type`)} = ${RubyUtil.wrapInQuotes(ut.`type`)} unless defined?(${RubyUtil.toUnionConstant(union, ut.`type`)})"
        }.mkString("\n").indent(2),
        "end"
      ).mkString("\n"),

      Seq(
        "def initialize(incoming={})",
        "  opts = HttpClient::Helper.symbolize_keys(incoming)",
        "  @name = HttpClient::Preconditions.assert_class('name', opts.delete(:name), String)",
        "end"
      ).mkString("\n"),

      Seq(
        "def to_hash",
        "  { @name => subtype_to_hash }",
        "end"
      ).mkString("\n"),

      Seq(
        s"def $className.from_json(hash)",
        s"  HttpClient::Preconditions.assert_class('hash', hash, Hash)",
        s"  hash.map do |union_type_name, data|",
        s"    case union_type_name",
        union.types.map { ut =>
          Primitives(ut.`type`) match {
            case None => s"when Types::${RubyUtil.toUnionConstant(union, ut.`type`)}; ${RubyUtil.toClassName(ut.`type`)}.new(data)"
            case Some(p) => s"when Types::${RubyUtil.toUnionConstant(union, ut.`type`)}; ${RubyPrimitiveWrapper.className(union, p)}.new(data)"
          }
        }.mkString("\n").indent(6),
        s"      else ${undefinedTypeClassName(union)}.new(:name => union_type_name)",
        s"    end",
        s"  end.first",
        s"end"
      ).mkString("\n")

    ).mkString("\n\n").indent(2) +
    "\n\nend"
  }

  def generateUnionUndefinedType(union: Union): String = {
    val className = undefinedTypeClassName(union)
    Seq(
      s"class $className < ${RubyUtil.toClassName(union.name)}",

      Seq(
        "attr_reader :name",

        Seq(
          "def initialize(incoming={})",
	  "  super(:name => 'undefined_type')",
          "  opts = HttpClient::Helper.symbolize_keys(incoming)",
          "  @name = HttpClient::Preconditions.assert_class('name', opts.delete(:name), String)",
          "end"
        ).mkString("\n"),

        Seq(
          "def subtype_to_hash",
          "  raise 'Unable to serialize undefined type to json'",
          "end"
        ).mkString("\n"),

        Seq(
          "def copy(incoming={})",
          "  raise 'Operation not supported for undefined type'",
          "end"
        ).mkString("\n"),

        Seq(
          "def to_hash",
          "  raise 'Operation not supported for undefined type'",
          "end"
        ).mkString("\n")

      ).mkString("\n\n").indent(2),

      "end"
    ).mkString("\n\n")
  }

  def generateModel(model: Model, union: Option[Union]): String = {
    val className = RubyUtil.toClassName(model.name)

    val sb = ListBuffer[String]()

    model.description.map { desc => sb.append(GeneratorUtil.formatComment(desc)) }
    sb.append(RubyClientGenerator.classDeclaration(className, union.map(u => RubyUtil.toClassName(u.name))) + "\n")

    sb.append("  attr_reader " + model.fields.map( f => s":${f.name}" ).mkString(", "))

    sb.append("")
    sb.append("  def initialize(incoming={})")
    union.map { u =>
      sb.append(s"    super(:name => ${RubyUtil.toClassName(u.name)}::Types::${RubyUtil.toUnionConstant(u, model.name)})")
    }

    sb.append("    opts = HttpClient::Helper.symbolize_keys(incoming)")

    model.fields.map { field =>
      sb.append(s"    @${field.name} = ${parseArgument(field.name, field.`type`, field.required, field.default)}")
    }

    sb.append("  end\n")

    sb.append("  def to_json")
    sb.append("    JSON.dump(to_hash)")
    sb.append("  end\n")

    val toHashMethodName = union match {
      case None => "to_hash"
      case Some(_) => "subtype_to_hash"
    }

    sb.append("  def copy(incoming={})")
    sb.append(s"    $className.new(${toHashMethodName}.merge(HttpClient::Helper.symbolize_keys(incoming)))")
    sb.append("  end\n")

    sb.append(s"  def $toHashMethodName")
    sb.append("    {")
    sb.append(
      model.fields.map { field =>
        val datatype = parseType(field.`type`).datatype
        val value = datatype match {
          case Datatype.Singleton(_) => {
            datatype.`type` match {
              case Type(Kind.Primitive, name) => {
                Primitives(name) match {
                  case Some(Primitives.Object) | Some(Primitives.Unit) => field.name
                  case _ => asString(field.name, name, escape = false)
                }
              }
              case Type(Kind.Model | Kind.Union, name) => {
                s"${field.name}.nil? ? nil : ${field.name}.to_hash"
              }
              case Type(Kind.Enum, name) => {
                s"${field.name}.nil? ? nil : ${field.name}.value"
              }
            }
          }
          case Datatype.List(t) => {
            t match {
              case Type(Kind.Primitive, name) => {
                Primitives(name) match {
                  case Some(Primitives.Object) | Some(Primitives.Unit) => field.name
                  case _ => asString(field.name, name, escape = false)
                }
              }
              case Type(Kind.Model | Kind.Union, name) => {
                s"(${field.name} || []).map(&:to_hash)"
              }
              case Type(Kind.Enum, name) => {
                s"(${field.name} || []).map(&:value)"
              }
            }
          }

          case Datatype.Map(t) => {
            t match {
              case Type(Kind.Primitive, name) => {
                Primitives(name) match {
                  case Some(Primitives.Object) | Some(Primitives.Unit) => field.name
                  case _ => asString(field.name, name, escape = false)
                }
              }
              case Type(Kind.Model | Kind.Union, name) => {
                s"(${field.name} || {}).inject({}).map { |h, o| h[o[0]] = o[1].nil? ? nil : o[1].to_hash; h }"
              }
              case Type(Kind.Enum, name) => {
                s"(${field.name} || {}).inject({}).map { |h, o| h[o[0]] = o[1].nil? ? nil : o[1].value; h }"
              }
            }
          }
        }

        s":${field.name} => ${value}"
      }.mkString(",\n").indent(6)
    )
    sb.append("    }")
    sb.append("  end\n")

    sb.append("end")

    sb.mkString("\n")
  }

  private def withHelper(
    method: String,
    code: String
  ) = s"$method($code)"

  private def parseArgumentPrimitive(
    fieldName: String,
    value: String,
    ptName: String,
    required: Boolean,
    default: Option[String]
  ): String = {
    val pt = Primitives(ptName).getOrElse {
      sys.error(s"Invalid primitive[$ptName]")
    }
    val arg = pt match {
      case Primitives.String | Primitives.Integer | Primitives.Double | Primitives.Long | Primitives.Boolean => {
        value
      }

      case Primitives.DateIso8601 => {
        withHelper("HttpClient::Helper.to_date_iso8601", value)
      }

      case Primitives.DateTimeIso8601 => {
        withHelper("HttpClient::Helper.to_date_time_iso8601", value)
      }

      case Primitives.Uuid => {
        withHelper("HttpClient::Helper.to_uuid", value)
      }

      case Primitives.Decimal => {
        withHelper("HttpClient::Helper.to_big_decimal", value)
      }

      case Primitives.Object => {
        withHelper("HttpClient::Helper.to_object", value)
      }

      case Primitives.Unit => {
        sys.error("Cannot have a unit parameter")
      }
    }

    val mustBeSpecified = required && default.isEmpty

    (pt, mustBeSpecified) match {
      case (Primitives.Boolean, true) => {
        s"HttpClient::Preconditions.assert_boolean('$fieldName', $arg)"
      }
      case (Primitives.Boolean, false) => {
        s"HttpClient::Preconditions.assert_boolean_or_nil('$fieldName', $arg)"
      }
      case (_, req) => {
        val className = rubyClass(pt)
        val assertMethod = if (req) { "assert_class" } else { "assert_class_or_nil" }
        s"HttpClient::Preconditions.$assertMethod('$fieldName', $arg, $className)"
      }
    }
  }

  private def withDefaultArray(
    fieldName: String,
    arg: String,
    required: Boolean
  ) = required match {
    case false => s"($arg || [])"
    case true => s"HttpClient::Preconditions.assert_class('$fieldName', $arg, Array)"
  }

  private def withDefaultMap(
    fieldName: String,
    arg: String,
    required: Boolean
  ) = required match {
    case false => s"($arg || {})"
    case true => s"HttpClient::Preconditions.assert_class('$fieldName', $arg, Hash)"
  }

  private def parseArgument(
    fieldName: String,
    `type`: String,
    required: Boolean,
    default: Option[String]
  ): String = {
    val dt = datatypeResolver.parse(`type`).getOrElse {
      sys.error("Invalid type[" + `type` + "]")
    }

    dt match {
      case Datatype.Singleton(_) => {
        dt.`type` match {
          case Type(Kind.Primitive, name) => {
            parseArgumentPrimitive(fieldName, s"opts.delete(:$fieldName)", name, required, default)
          }
          case Type(Kind.Model, name) => {
            val klass = qualifiedClassName(name)
            wrapWithAssertion(
              fieldName,
              klass,
              required,
              s"opts[:$fieldName].nil? ? nil : (opts[:$fieldName].is_a?($klass) ? opts.delete(:$fieldName) : $klass.new(opts.delete(:$fieldName)))"
            )
          }
          case Type(Kind.Union, name) => {
            val klass = qualifiedClassName(name)
            wrapWithAssertion(
              fieldName,
              klass,
              required,
              s"opts[:$fieldName].nil? ? nil : (opts[:$fieldName].is_a?($klass) ? opts.delete(:$fieldName) : $klass.from_json(opts.delete(:$fieldName)))"
            )
          }
          case Type(Kind.Enum, name) => {
            val klass = qualifiedClassName(name)
            wrapWithAssertion(
              fieldName,
              klass,
              required,
              s"opts[:$fieldName].nil? ? nil : (opts[:$fieldName].is_a?($klass) ? opts.delete(:$fieldName) : $klass.apply(opts.delete(:$fieldName)))"
            )
          }
        }
      }

      case Datatype.List(t) => {
        t match {
          case Type(Kind.Primitive, name) => {
            withDefaultArray(fieldName, s"opts.delete(:$fieldName)", required) + ".map { |v| " + parseArgumentPrimitive(fieldName, "v", name, required, default) + "}"
          }
          case Type(Kind.Model, name) => {
            val klass = qualifiedClassName(name)
            withDefaultArray(fieldName, s"opts.delete(:$fieldName)", required) + ".map { |el| " + s"el.nil? ? nil : (el.is_a?($klass) ? el : $klass.new(el)) }"
          }
          case Type(Kind.Union, name) => {
            val klass = qualifiedClassName(name)
            withDefaultArray(fieldName, s"opts.delete(:$fieldName)", required) + ".map { |el| " + s"el.nil? ? nil : (el.is_a?($klass) ? el : $klass.from_json(el)) }"
          }
          case Type(Kind.Enum, name) => {
            val klass = qualifiedClassName(name)
            withDefaultArray(fieldName, s"opts.delete(:$fieldName)", required) + s".map { |el| el.nil? ? nil : (el.is_a?($klass) ? el : $klass.apply(el)) }"
          }
        }
      }

      case Datatype.Map(t) => {
        t match {
          case Type(Kind.Primitive, name) => {
            withDefaultMap(fieldName, s"opts.delete(:$fieldName)", required) + ".inject({}) { |h, d| h[d[0]] = " + parseArgumentPrimitive(fieldName, "d[1]", name, required, default) + "; h }"
          }
          case Type(Kind.Model, name) => {
            val klass = qualifiedClassName(name)
            withDefaultMap(fieldName, s"opts.delete(:$fieldName)", required) + ".inject({}) { |h, el| h[el[0]] = " + s"el[1].nil? ? nil : (el[1].is_a?($klass) ? el[1] : $klass.new(el[1])); h" + "}"
          }
          case Type(Kind.Union, name) => {
            val klass = qualifiedClassName(name)
            withDefaultMap(fieldName, s"opts.delete(:$fieldName)", required) + ".inject({}) { |h, el| h[el[0]] = " + s"el[1].nil? ? nil : (el[1].is_a?($klass) ? el[1] : $klass.from_json(el[1])); h" + "}"
          }
          case Type(Kind.Enum, name) => {
            val klass = qualifiedClassName(name)
            wrapWithAssertion(
              fieldName,
              klass,
              required,
              s"opts[:$fieldName].nil? ? nil : (opts[:$fieldName].is_a?($klass) ? opts.delete(:$fieldName) : $klass.apply(opts.delete(:$fieldName)))"
            )
          }
        }
      }
    }
  }

  private def wrapWithAssertion(
    fieldName: String,
    className: String,
    required: Boolean,
    code: String
  ): String = {
    val assertMethod = if (required) { "assert_class" } else { "assert_class_or_nil" }
    s"HttpClient::Preconditions.$assertMethod('$fieldName', $code, $className)"
  }

  private def qualifiedClassName(
    name: String
  ): String = {
    // If the name is already full qualified (e.g. com.foo.bar), then
    // use the name directly. Otherwise add this service's module
    // name.
    val thisModule = RubyUtil.Module(name)
    if (thisModule.parts.size <= 1) {
      "%s::Models::%s".format(
        module.fullName,
        RubyUtil.toClassName(name)
      )
    } else {
      thisModule.fullName
    }
  }

  private def rubyClass(pt: Primitives): String = {
    pt match {
      case Primitives.Boolean => "String"
      case Primitives.Decimal => "BigDecimal"
      case Primitives.Double => "Float"
      case Primitives.Integer => "Integer"
      case Primitives.Long => "Integer"
      case Primitives.DateIso8601 => "Date"
      case Primitives.DateTimeIso8601 => "DateTime"
      case Primitives.Object => "Hash"
      case Primitives.String => "String"
      case Primitives.Unit => "nil"
      case Primitives.Uuid => "String"
    }
  }

  private def asString(
    varName: String,
    ptName: String,
    escape: Boolean
  ): String = {
    Primitives(ptName).getOrElse {
      sys.error(s"Unknown primitive type[$ptName]")
    } match {
      case Primitives.Integer | Primitives.Double | Primitives.Long | Primitives.Uuid | Primitives.Decimal | Primitives.Boolean => varName
      case Primitives.String => {
        if (escape) {
          s"CGI.escape($varName)"
        } else {
          varName
        }
      }
      case Primitives.DateIso8601 => s"HttpClient::Helper.date_iso8601_to_string($varName)"
      case Primitives.DateTimeIso8601 => s"HttpClient::Helper.date_time_iso8601_to_string($varName)"
      case Primitives.Object | Primitives.Unit => {
        sys.error(s"Unsupported type[$ptName] for string formatting - varName[$varName]")
      }
    }
  }

  case class RubyTypeInfo(
    varName: String,
    klass: String,
    assertMethod: String,
    datatype: Datatype
  )

  private def parseType(
    `type`: String,
    fieldName: Option[String] = None
  ): RubyTypeInfo = {
    val dt = datatypeResolver.parse(`type`).getOrElse {
      sys.error("Invalid type[" + `type` + "]")
    }

    val klass = dt.`type` match {
      case Type(Kind.Primitive, ptName) => Primitives(ptName) match {
        case None => sys.error(s"Unsupported primitive[$ptName] for type[${dt.label}]")
        case Some(pt) => rubyClass(pt)
      }
      case Type(Kind.Model | Kind.Enum | Kind.Union, name) => qualifiedClassName(name)
    }

    val multiple = dt match {
      case Datatype.Singleton(_) => false
      case Datatype.List(_) | Datatype.Map(_) => true
    }

    val varName = dt.`type` match {
      case Type(Kind.Primitive, name) => {
        fieldName match {
          case Some(n) => RubyUtil.toVariable(n, multiple = multiple)
          case None => RubyUtil.toDefaultVariable(multiple = multiple)
        }
      }
      case Type(Kind.Model | Kind.Enum | Kind.Union, name) => {
        RubyUtil.toVariable(fieldName.getOrElse(name), multiple = multiple)
      }
    }

    val assertStub = dt match {
      case Datatype.Singleton(_) => "assert_class"
      case Datatype.List(_) => "assert_collection_of_class"
      case Datatype.Map(_) => "assert_hash_of_class"
    }

    RubyTypeInfo(
      varName = varName,
      klass = klass,
      assertMethod = s"HttpClient::Preconditions.$assertStub('$varName', $varName, $klass)",
      datatype = dt
    )
  }

  def generateResponses(op: Operation): String = {
    // TODO: match on all response codes
    op.responses.sortWith { _.code < _.code }.headOption.flatMap { generateResponse(_) }.getOrElse("\n        nil")
  }

  def generateResponse(response: Response): Option[String] = {
    val dt = parseType(response.`type`).datatype

    dt.`type` match {
      case Type(Kind.Primitive, name) => {
        Some(buildResponse(Container(dt), RubyUtil.toDefaultVariable()))
      }
      case Type(Kind.Model | Kind.Enum, name) => {
        Some(buildResponse(Container(dt), name))
      }
      case Type(Kind.Union, name) => {
        Some(buildResponse(Container(dt), name, "from_json"))
      }
    }
  }

  private def buildResponse(
    container: Container,
    name: String,
    constructor: String = "new"
  ): String = {
    val varName = qualifiedClassName(name)
    val mapSingleObject = s" { |hash| $varName.$constructor(hash) }"
    container match {
      case Container.Singleton => {
        mapSingleObject
      }
      case Container.List => {
        ".map" + mapSingleObject
      }
      case Container.Map => {
        s".inject({}) { |hash, o| hash[o[0]] = o[1].nil? ? nil : $varName.new(hash); hash }"
      }
    }
  }

}
