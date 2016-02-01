package ruby.models

import java.util.UUID
import scala.util.Failure
import scala.util.Success
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import com.bryzek.apidoc.spec.v0.models._
import org.joda.time.format.ISODateTimeFormat.dateTimeParser
import lib.{Datatype, Methods, Text, VersionTag}
import lib.Text._
import lib.generator.{CodeGenerator, GeneratorUtil}

import scala.collection.mutable.ListBuffer
import play.api.Logger
import play.api.libs.json._
import generator.ServiceFileNames

object RubyUtil {

  private[this] val ReservedWords = Seq(
    "alias", "and", "begin", "break", "case", "class", "def", "defined?", "do",
    "else", "elsif", "end", "ensure", "false", "for", "if", "module",
    "next, nil, not", "or", "redo, rescue", "retry", "return", "self, super",
    "then, true", "undef", "unless", "until", "when", "while", "yield",
    "__FILE__", "__LINE__"
  ).toSet

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

  def quoteNameIfKeyword(name: String): String = {
    if (ReservedWords.contains(name)) {
      name + "_"
    } else {
      name
    }
  }

  def toMethodName(
    name: String
  ): String = {
    quoteNameIfKeyword(
      lib.Text.safeName(
        lib.Text.initLowerCase(lib.Text.splitIntoWords(name).map(_.toLowerCase).mkString("_"))
      )
    )
  }

  def toClassName(
    name: String,
    multiple: Boolean = false
  ): String = {
    val baseName =lib.Text.safeName(
      if (name == name.toUpperCase) {
       lib.Text.initCap(lib.Text.splitIntoWords(name).map(_.toLowerCase)).mkString("")
      } else {
       lib.Text.initCap(snakeToCamelCase(name))
      }
    )

    quoteNameIfKeyword(if (multiple) lib.Text.pluralize(baseName) else baseName)
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
    Datatype.Primitive(name) match {
      case Failure(_) => RubyUtil.toConstant(name)
      case Success(p) => RubyUtil.toConstant(RubyPrimitiveWrapper.className(union, p))
    }
  }

  def toDefaultVariable(
    multiple: Boolean = false
  ): String = {
    toVariable("value", multiple = multiple)
  }

  def toVariable(datatype: Datatype): String = {
    datatype match {
      case c: Datatype.Container => lib.Text.pluralize(toVariable(c.inner))
      case u: Datatype.UserDefined => RubyUtil.toVariable(u.name)
      case _: Datatype.Primitive => RubyUtil.toDefaultVariable()
    }
  }

  def toVariable(
    name: String,
    multiple: Boolean = false
  ): String = {
    val value = lib.Text.safeName(
      lib.Text.initLowerCase(lib.Text.splitIntoWords(name).map(_.toLowerCase).mkString("_"))
    )
    quoteNameIfKeyword(
      multiple match {
        case true => lib.Text.pluralize(value)
        case false => value
      }
    )
  }

  def wrapInQuotes(value: String): String = {
    if (value.indexOf("'") < 0) {
      s"'$value'"
    } else if (value.indexOf("\"") < 0) {
      s""""$value""""
    } else {
      sys.error(s"Unable to quote value[$value]")
    }
  }

  // TODO should be encapsulated in the RubyDatatype model
  def rubyDefault(value: String, datatype: Datatype): String = try {
    import Datatype._
    datatype match {
      case Primitive.String |
        Primitive.DateIso8601 |
        Primitive.DateTimeIso8601 |
        Primitive.Uuid => rubyDefault(JsString(value), datatype)
      case _: Primitive => rubyDefault(Json.parse(value), datatype)
      case _: Datatype.UserDefined.Enum => {
        rubyDefault(JsString(value), datatype)
      }
      case _ => rubyDefault(Json.parse(value), datatype)
    }
  } catch {
    case e: Exception => {
      throw new RuntimeException(s"parsing default `$value` for datatype $datatype", e)
    }
  }

  // TODO should be encapsulated in the RubyDatatype model
  private def rubyDefault(json: JsValue, datatype: Datatype): String = {
    import Datatype._
    datatype match {
      case Container.Option(inner) => 
        sys.error(s"parsing default `${json}` for datatype ${datatype}")
      case Container.List(inner) => {
        val seq = json.as[Seq[JsValue]].map { value =>
          rubyDefault(value, inner)
        }
        seq.mkString("[", ",", "]")
      }
      case Container.Map(inner) => {
        val map = json.as[Map[String, JsValue]].map { case (key, value) =>
          s""""${key}" => ${rubyDefault(value, inner)}"""
        }
        map.mkString("{", ",", "}")
      }
      case Primitive.Boolean => json.as[Boolean].toString
      case Primitive.Decimal => json.as[BigDecimal].toString
      case Primitive.Integer => json.as[Int].toString
      case Primitive.Double => json.as[Double].toString
      case Primitive.Long => json.as[Long].toString
      case Primitive.String => s""""${json.as[String]}""""
      case Primitive.DateIso8601 => {
        // validate the input
        val dt = dateTimeParser.parseLocalDate(json.as[String])
        s"Date.parse(${json})"
      }
      case Primitive.DateTimeIso8601 => {
        // validate the input
        val dt = dateTimeParser.parseDateTime(json.as[String])
        s"""DateTime.parse(${json})"""
      }
      case Primitive.Uuid => s"""UUID.new("${json.as[UUID]}")"""
      case Datatype.UserDefined.Enum(name)  => {
        val value = json.as[String]
        s"""${toClassName(name)}.apply("${value}")"""
      }
      case Datatype.UserDefined.Model(_) | Datatype.UserDefined.Union(_) |
        Primitive.Object | Primitive.Unit => {
        throw new UnsupportedOperationException(
          s"unsupported default for type ${datatype.name}")
      }
    }
  }
}

// TODO this should really be refactored to be more like
// the scala generator in two key respects
// 1) A ruby-specific wrapper around Datatype to provide
//    easy to update and understand methods in place
//    of most or all of the existing match statements.
//    See things commened about RubyDatatype
// 2) A ruby-specific wrapper around the service description.
//    The scala generator analogue to this provides a nice
//    layer of abstraction in which the data types are parsed,
//    allowing us to easily avoid parsing them multiple times,
//    as is currently the case in the ruby generator.
//    It would also do things like call quoteNameIfKeyword on
//    field names. Currently this is done in several places.
//    This approach is brittle, because when working with fields
//    in a new place, one must remember to use that method.
object RubyClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
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
  private[this] val service = form.service

  private[this] val module = RubyUtil.Module(service.namespace)

  private[this] val datatypeResolver = GeneratorUtil.datatypeResolver(service)

  private[this] val primitiveWrapper = RubyPrimitiveWrapper(service)

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

  def invoke(): Either[Seq[String], Seq[File]] = {
    Right(generateCode())
  }

  private def generateCode(): Seq[File] = {
    val spacerSize = 2
    val moduleIndent = spacerSize * module.parts.size

    val source = Seq(
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

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Client", source, Some("Ruby")))
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

  attr_reader :url

  def initialize(url, opts={})
    @url = HttpClient::Preconditions.assert_class('url', url, String)
    @authorization = HttpClient::Preconditions.assert_class_or_nil('authorization', opts.delete(:authorization), HttpClient::Authorization)
    @default_headers = HttpClient::Preconditions.assert_class('default_headers', opts.delete(:default_headers) || {}, Hash)
    HttpClient::Preconditions.assert_empty_opts(opts)
    HttpClient::Preconditions.check_state(url.match(/http.+/i), "URL[%s] must start with http" % url)
  end

  def request(path=nil)
    HttpClient::Preconditions.assert_class_or_nil('path', path, String)
    request = HttpClient::Request.new(URI.parse(@url + path.to_s))$headerString

    @default_headers.each do |key, value|
      request = request.with_header(key, value)
    end

    if @authorization
      request = request.with_auth(@authorization)
    end

    request
  end
""")

    sb.append(service.resources.map { resource =>
      val className = RubyUtil.toClassName(resource.plural)
      val varName = RubyUtil.toMethodName(resource.plural)

      s"  def $varName\n" +
      s"    @$varName ||= ${module.fullName}::Clients::${className}.new(self)\n" +
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
          val dt = datatypeResolver.parse(param.`type`, true).getOrElse {
            sys.error(s"Invalid type[${param.`type`}]")
          }
          dt match {
            case p: Datatype.Primitive => {
              val code = asString(RubyUtil.toVariable(varName), p, escape = true)
              s"#{$code}"
            }
            case e: Datatype.UserDefined.Enum => {
              val code = RubyUtil.toVariable(varName)
              s"#{$code.value}"
            }
            case _ => {
              sys.error(s"Model ${dt.name} is not a valid path parameter type")
            }
          }
        } else {
          name
        }
      }.mkString("/")

      val methodName =lib.Text.camelCaseToUnderscore(
        GeneratorUtil.urlToMethodName(
          resource.path,
          resource.operations.map(_.path),
          op.method,
          op.path
        )
      ).toLowerCase

      val paramStrings = ListBuffer[String]()
      pathParams.map(_.name).foreach { n => paramStrings.append(RubyUtil.toVariable(n)) }

      if (Methods.isJsonDocumentMethod(op.method.toString)) {
        op.body.flatMap(body => datatypeResolver.parse(body.`type`, true).toOption) match {
          case None => paramStrings.append("hash")
          case Some(dt) => {
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

            ti.datatype match {
              case p: Datatype.Primitive => requestBuilder.append(s".with_body(${ti.varName})")
              case dt => requestBuilder.append(s".with_json(${asJson(ti.varName, dt)})")
            }
          }
          case _ => sys.error(s"Invalid body [${op.body}]")
        }
      }
      requestBuilder.append(s".${op.method.toString.toLowerCase}")

      sb.append(s"    r = ${requestBuilder.toString}")
      sb.append(s"    ${generateResponses(op, "r")}")
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
          ut.description.map { desc => GeneratorUtil.formatComment(desc) + "\n" }.getOrElse("") +
          s"${RubyUtil.toUnionConstant(union, ut.`type`)} = ${RubyUtil.wrapInQuotes(ut.`type`)} unless defined?(${RubyUtil.toUnionConstant(union, ut.`type`)})"
        }.mkString("\n").indent(2),
        "end"
      ).mkString("\n"),

      Seq(
        "def initialize(incoming={})",
        "  opts = HttpClient::Helper.symbolize_keys(incoming)",
        s"  HttpClient::Preconditions.require_keys(opts, [:name], '$className')",
        "  @name = HttpClient::Preconditions.assert_class('name', opts.delete(:name), String)",
        "end"
      ).mkString("\n"),

      generateUnionClassToHash(union),

      generateUnionClassFromJson(union)

    ).mkString("\n\n").indent(2) +
    "\n\nend"
  }

  private[this] def generateUnionClassToHash(union: Union): String = {
    Seq(
      "def to_hash",
      union.discriminator match {
        case None => {
          "  { @name => subtype_to_hash }"
        }
        case Some(disc) => {
          s"""  subtype_to_hash.merge(:$disc => @name)"""
        }
      },
      "end"
    ).mkString("\n")
  }

  private[this] def generateUnionClassFromJson(union: Union): String = {
    val className = RubyUtil.toClassName(union.name)

    union.discriminator match {
      case None => {
        Seq(
          s"def $className.from_json(hash)",
          s"  HttpClient::Preconditions.assert_class('hash', hash, Hash)",
          s"  hash.map do |union_type_name, data|",
          s"    case union_type_name",
          union.types.map { ut =>
            Datatype.Primitive(ut.`type`) match {
              case Failure(_) => s"when Types::${RubyUtil.toUnionConstant(union, ut.`type`)}; ${RubyUtil.toClassName(ut.`type`)}.new(data)"
              case Success(p) => s"when Types::${RubyUtil.toUnionConstant(union, ut.`type`)}; ${RubyPrimitiveWrapper.className(union, p)}.new(data)"
            }
          }.mkString("\n").indent(6),
          s"      else ${undefinedTypeClassName(union)}.new(:name => union_type_name)",
          s"    end",
          s"  end.first",
          s"end"
        ).mkString("\n")
      }
      case Some(disc) => {
        Seq(
          s"def $className.from_json(hash)",
          s"  HttpClient::Preconditions.assert_class('hash', hash, Hash)",
          s"  case HttpClient::Helper.symbolize_keys(hash)[:$disc]",
          union.types.map { ut =>
            Datatype.Primitive(ut.`type`) match {
              case Failure(_) => s"when Types::${RubyUtil.toUnionConstant(union, ut.`type`)}; ${RubyUtil.toClassName(ut.`type`)}.new(hash)"
              case Success(p) => s"when Types::${RubyUtil.toUnionConstant(union, ut.`type`)}; ${RubyPrimitiveWrapper.className(union, p)}.new(hash)"
            }
          }.mkString("\n").indent(4),
          s"    else ${undefinedTypeClassName(union)}.new(:name => union_type_name)",
          s"  end",
          s"end"
        ).mkString("\n")
      }
    }
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

    sb.append("  attr_reader " + model.fields.map( f => s":${RubyUtil.quoteNameIfKeyword(f.name)}" ).mkString(", "))

    sb.append("")
    sb.append("  def initialize(incoming={})")
    union.map { u =>
      sb.append(s"    super(:name => ${RubyUtil.toClassName(u.name)}::Types::${RubyUtil.toUnionConstant(u, model.name)})")
    }

    sb.append("    opts = HttpClient::Helper.symbolize_keys(incoming)")

    model.fields.filter( f => f.required && f.default.isEmpty ) match {
      case Nil => {}
      case required => {
        sb.append("    HttpClient::Preconditions.require_keys(opts, [" + required.map { f => s":${RubyUtil.quoteNameIfKeyword(f.name)}" }.mkString(", ") + s"], '$className')")
      }
    }

    model.fields.map { field =>
      val varName = RubyUtil.quoteNameIfKeyword(field.name)
      sb.append(s"    @$varName = ${parseArgument(field.name, field.`type`, field.required, field.default)}")
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
        val datatype = datatypeResolver.parse(field.`type`, field.required).get
        val varName = RubyUtil.quoteNameIfKeyword(field.name)
        val value = asHash(varName, datatype)
        s":${field.name} => ${value}"
      }.mkString(",\n").indent(6)
    )
    sb.append("    }")
    sb.append("  end\n")

    sb.append("end")

    sb.mkString("\n")
  }

  private def parseArgument(
    fieldName: String,
    `type`: String,
    required: Boolean,
    rawDefault: Option[String]
  ): String = {
    val dt = datatypeResolver.parse(`type`, required).getOrElse {
      sys.error("Invalid type[" + `type` + "]")
    }
    val default = rawDefault.map(RubyUtil.rubyDefault(_, dt))
    parseArgument(fieldName, s"opts.delete(:${fieldName})", dt, default)
  }

  // TODO should be encapsulated in the RubyDatatype model
  private def parseArgument(
    name: String, rawExpr: String,
    dt: Datatype, default: Option[String]
  ): String = {
    import Datatype._
    def expr = default.fold(rawExpr)(d => s"(x = ${rawExpr}; x.nil? ? ${d} : x)")
    dt match {
      case Primitive.Boolean =>
        s"HttpClient::Preconditions.assert_boolean('$name', ${expr})"

      case Primitive.Unit =>
        sys.error("Cannot have a unit parameter")

      case p: Primitive => {
        val (helperExpr, className) = p match {
          case Primitive.Boolean | Primitive.String | Primitive.Integer | Primitive.Double | Primitive.Long | Primitive.Unit =>
            (expr, rubyClass(p))

          case Primitive.DateIso8601 =>
            (s"HttpClient::Helper.to_date_iso8601(${expr})", "Date")

          case Primitive.DateTimeIso8601 =>
            (s"HttpClient::Helper.to_date_time_iso8601(${expr})", "DateTime")

          case Primitive.Uuid =>
            (s"HttpClient::Helper.to_uuid(${expr})", "String")

          case Primitive.Decimal =>
            (s"HttpClient::Helper.to_big_decimal(${expr})", "BigDecimal")

          case Primitive.Object =>
            (s"HttpClient::Helper.to_object(${expr})", "Hash")
        }

        s"HttpClient::Preconditions.assert_class('$name', ${helperExpr}, ${className})"
      }

      case m: UserDefined.Model =>
        val className = qualifiedClassName(m.name)
        s"(x = ${expr}; x.is_a?(${className}) ? x : ${className}.new(x))"

      case e: UserDefined.Enum =>
        val className = qualifiedClassName(e.name)
        s"(x = ${expr}; x.is_a?(${className}) ? x : ${className}.apply(x))"

      case u: UserDefined.Union =>
        val className = qualifiedClassName(u.name)
        s"(x = ${expr}; x.is_a?(${className}) ? x : ${className}.from_json(x))"

      case Container.List(inner) =>
        s"HttpClient::Preconditions.assert_class('$name', ${expr}, Array).map { |v| ${parseArgument(name, "v", inner, None)} }"

      case Container.Map(inner) =>
        s"HttpClient::Preconditions.assert_class('$name', ${expr}, Hash).inject({}) { |h, d| h[d[0]] = ${parseArgument(name, "d[1]", inner, None)}; h }"

      case Container.Option(inner) =>
        s"(x = ${expr}; x.nil? ? nil : ${parseArgument(name, "x", inner, None)})"
    }
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

  // TODO should be encapsulated in the RubyDatatype model
  private def rubyClass(pt: Datatype.Primitive): String = {
    import Datatype.Primitive
    pt match {
      case Primitive.Boolean => "String"
      case Primitive.Decimal => "BigDecimal"
      case Primitive.Double => "Float"
      case Primitive.Integer => "Integer"
      case Primitive.Long => "Integer"
      case Primitive.DateIso8601 => "Date"
      case Primitive.DateTimeIso8601 => "DateTime"
      case Primitive.Object => "Hash"
      case Primitive.String => "String"
      case Primitive.Unit => "nil"
      case Primitive.Uuid => "String"
    }
  }

  // TODO should be encapsulated in the RubyDatatype model
  private def asString(
    varName: String,
    pt: Datatype.Primitive,
    escape: Boolean
  ): String = {
    import Datatype.Primitive
    pt match {
      case Primitive.Integer | Primitive.Double | Primitive.Long | Primitive.Uuid | Primitive.Decimal | Primitive.Boolean => varName
      case Primitive.String => {
        if (escape) {
          s"CGI.escape($varName)"
        } else {
          varName
        }
      }
      case Primitive.DateIso8601 => s"HttpClient::Helper.date_iso8601_to_string($varName)"
      case Primitive.DateTimeIso8601 => s"HttpClient::Helper.date_time_iso8601_to_string($varName)"
      case Primitive.Object | Primitive.Unit => {
        sys.error(s"Unsupported type[${pt.name}] for string formatting - varName[$varName]")
      }
    }
  }

  // TODO should be encapsulated in the RubyDatatype model
  private def asJson(varName: String, dt: Datatype): String = {
    import Datatype._
    val hash = dt match {
      case _: Datatype.Primitive => varName
      case Datatype.Container.List(_: Datatype.Primitive) => varName
      case Datatype.Container.Map(_: Datatype.Primitive) => varName
      case Datatype.Container.Option(_: Datatype.Primitive) => varName

      case Datatype.Container.List(inner) =>
        s"${varName}.map { |o| ${asHash("o", inner)} }"

      case Datatype.Container.Map(inner) =>
        s"${varName}.inject({}) { |hash, o| hash[o[0]] = o[1].nil? ? nil : ${asHash("o[1]", inner)}; hash }"

      case Datatype.Container.Option(inner) =>
        s"${varName}.nil? ? nil : ${asJson(varName, inner)}"

      case _: Datatype.UserDefined => s"${varName}"
    }
    s"${hash}.to_json"
  }

  // TODO should be encapsulated in the RubyDatatype model
  private def asHash(varName: String, dt: Datatype): String = {
    import Datatype._
    dt match {
      case _: Datatype.Primitive => varName
      case Datatype.Container.List(_: Datatype.Primitive) => varName
      case Datatype.Container.Map(_: Datatype.Primitive) => varName
      case Datatype.Container.Option(_: Datatype.Primitive) => varName

      case Datatype.Container.List(inner) =>
        s"${varName}.map { |o| ${asHash("o", inner)} }"

      case Datatype.Container.Map(inner) =>
        s"${varName}.inject({}) { |hash, o| hash[o[0]] = o[1].nil? ? nil : ${asHash("o[1]", inner)}; hash }"

      case Datatype.Container.Option(inner) =>
        s"${varName}.nil? ? nil : ${asHash(varName, inner)}"

      case Datatype.UserDefined.Enum(_) => s"${varName}.value"

      case _: Datatype.UserDefined => s"${varName}.to_hash"
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
    val dt = datatypeResolver.parse(`type`, true).get

    val klass = {
      def iter(dt: Datatype): String = dt match {
        case p: Datatype.Primitive => rubyClass(p)
        case u: Datatype.UserDefined => qualifiedClassName(u.name)
        case Datatype.Container.Option(inner) => iter(inner)
        case m: Datatype.Container.Map => "Hash"
        case l: Datatype.Container.List => "Array"
      }
      iter(dt)
    }

    val varName = dt match {
      case p: Datatype.Primitive => {
        fieldName match {
          case Some(n) => RubyUtil.toVariable(n)
          case None => RubyUtil.toDefaultVariable()
        }
      }
      case u: Datatype.UserDefined => {
        RubyUtil.toVariable(fieldName.getOrElse(u.name))
      }
      case c: Datatype.Container =>
        // TODO this doesn't support arbitrary nesting
        RubyUtil.toVariable(fieldName.getOrElse(c.inner.name), multiple = true)
    }

    val (assertStub, assertClass) = dt match {
      case Datatype.Container.List(inner) => ("assert_collection_of_class", qualifiedClassName(inner.name))
      case Datatype.Container.Map(inner) => ("assert_hash_of_class", qualifiedClassName(inner.name))
      case _ => ("assert_class", klass)
    }

    RubyTypeInfo(
      varName = varName,
      klass = klass,
      assertMethod = s"HttpClient::Preconditions.$assertStub('$varName', $varName, $assertClass)",
      datatype = dt
    )
  }

  def generateResponses(op: Operation, varName: String): String = {
    // TODO: match on all response codes
    op.responses.sortBy { r => Util.responseCodeAsString(r.code).toString }
      .headOption.map { r => generateResponse(r, varName) }
      .getOrElse("\n        nil")
  }

  def generateResponse(response: Response, varName: String): String = {
    val dt = parseType(response.`type`).datatype
    generateResponse(dt, varName)
  }

  // TODO should be encapsulated in the RubyDatatype model
  private def generateResponse(dt: Datatype, varName: String): String = {
    import Datatype._
    dt match {
      case p: Primitive => {
        p match {
          case Primitive.Unit => "nil"
          case Primitive.Boolean | Primitive.Decimal | Primitive.Double | Primitive.Integer | Primitive.Long | Primitive.DateIso8601 | Primitive.DateTimeIso8601 | Primitive.Object | Primitive.String | Primitive.Uuid => {
            s"${qualifiedClassName(RubyUtil.toDefaultVariable())}.new(${varName})"
          }
        }
      }
      case t @ (UserDefined.Model(_) | UserDefined.Enum(_)) =>
        s"${qualifiedClassName(t.name)}.new(${varName})"
      case UserDefined.Union(name) =>
        s"${qualifiedClassName(name)}.from_json(${varName})"
      case Container.List(inner) =>
        s"${varName}.map { |x| ${generateResponse(inner, "x")} }"
      case Container.Map(inner) =>
        // TODO code for this used to use pass hash to the constructor,
        // instead of x[1]. Pretty sure that was wrong, but hard to tell.
        s"${varName}.inject({}) { |hash, x| hash[x[0]] = x[1].nil? ? nil : ${generateResponse(inner, "x[1]")}; hash }"
      case Container.Option(inner) => sys.error(s"unsupported datatype ${dt} for response")
    }
  }

}
