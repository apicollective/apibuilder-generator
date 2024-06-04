package generator.elm

import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models._

import scala.annotation.tailrec

case class ElmResource(args: GenArgs) {
  private[this] val elmType = ElmTypeLookup(args)

  def generate(resource: Resource): ValidatedNec[String, String] = {
    args.imports.addAs("Env", "Env")

    resource.operations.map { op =>
      Generator(resource, op).generate()
    }.sequence.map(_.mkString("\n\n"))
  }

  case class Generator(resource: Resource, op: Operation) {
    private[this] val variableIndex = VariableIndex()
    private[this] val elmJson = ElmJson(args.imports)

    private[this] val name: String = {
      val (variables, words) = op.path.drop(resource.path.map(_.length).getOrElse(0)).split("/").partition(_.startsWith(":"))
      def toOpt(all: Seq[String]) = {
        all.map(Names.pascalCase).toList match {
          case Nil => None
          case names => Some(names)
        }
      }

      val prefix = op.method.toString.toLowerCase() + Names.pascalCase(resource.plural)
      Seq(
        Some(prefix),
        toOpt(words.toSeq).map(_.mkString("")),
        toOpt(variables.toSeq).map { w => "By" + w.mkString("And") }
      ).flatten.mkString("")
    }

    private[this] val propsType = Names.pascalCase(name) + "Props"

    private[this] def handlePossibleToString(params: Seq[ValidatedParameter], variable: String, code: String): String = {
      import ElmType._

      def wrap(fun: String): String = Util.wrapInParens(fun, Names.maybeQuote(code))

      params.find(_.name == variable) match {
        case None => code
        case Some(p) => p.typ match {
          case ElmString => code
          case ElmInt => wrap("String.fromInt")
          case ElmFloat => wrap("String.fromFloat")
          case ElmBool => wrap("boolToString")
          case ElmEnumLocal(name) => wrap(Names.camelCase(name) + "ToString")
          case ElmEnumImported(ns, name) => {
            args.imports.addExposing(ns, name)
            wrap(Names.camelCase(name) + "ToString")
          }
          case ElmNothing | ElmDate |
            ElmDict(_) |
            ElmList(_) |
            ElmMaybe(_) |
            ElmPosix |
            _: ElmUserDefinedLocal |
           _: ElmUserDefinedImported => sys.error(s"Do not know how to convert parameter named ${p.name} with type ${p.typ} to String")
        }
      }
    }

    private[this] def url(variable: ElmVariable, params: Seq[ValidatedParameter]): String = {
      @tailrec
      def buildUrl(remaining: String, u: Seq[String]): Seq[String] = {
        val i = remaining.indexOf(":")
        if (i < 0) {
          u ++ Seq(Util.wrapInQuotes(remaining))
        } else {
          val prefix = remaining.take(i)
          val endIndex = remaining.drop(i).indexOf("/")

          def gen(w: String) = {
            val code = w match {
              case ":community_id" => "Env.config.community.id" // TODO: Move to config
              case _ => {
                val bareWord = if (w.startsWith(":")) {
                  w.drop(1)
                } else {
                  w
                }
                val code = dereferenceVariable(variable, bareWord)
                handlePossibleToString(params, bareWord, code)
              }
            }
            u ++ Seq(Util.wrapInQuotes(prefix) + s" ++ $code")
          }
          if (endIndex < 0) {
            gen(remaining.drop(i))
          } else {
            buildUrl(
              remaining.drop(i+endIndex),
              gen(remaining.drop(i).take(endIndex))
            )
          }
        }
      }

      val url = buildUrl(op.path, Nil).mkString(" ++ ")
      params.filter(_.p.location == ParameterLocation.Query).toList match {
        case Nil => url
        case all => {
          val queryParams = queryParameters(variable, all)
          args.imports.addExposing("Url.Builder", "toQuery")
          s"String.append ${Util.maybeWrapInParens(url)} (toQuery(\n        $queryParams\n        ))"
        }
      }
    }

    /*
        [ string "sort" sort
        , int "limit" (lo.limit + 1)
        , int "offset" lo.offset
        ]
     */
    private[this] def queryParameters(variable: ElmVariable, params: Seq[ValidatedParameter]): String = {
      assert(params.nonEmpty, "Must have at least one param")
      params.map { p =>
        queryParameter(p)(dereferenceVariable(variable, p.name))
      }.mkString("\n++ ").indent(16)
    }

    private[this] def dereferenceVariable(variable: ElmVariable, name: String): String = {
      variable match {
        case _: ElmParameter => name
        case a: ElmTypeAlias => s"${a.name}.${Names.camelCase(name)}"
      }
    }

    private[this] def queryParameter(p: ValidatedParameter, functions: Seq[String] = Nil, depth: Int = 0)(currentVar: String): String = {
      import ElmType._
      lazy val nextVar = variableIndex.next()
      def innerType(inner: ElmType): String = {
        Util.maybeWrapInParens(queryParameter(p.copy(typ = inner), functions, depth = depth + 1)(nextVar))
      }

      def asString(function: String) = {
        val code = queryParameter(p.copy(typ = ElmString), functions = functions ++ Seq(function), depth = depth)(currentVar)
        if (depth == 0) {
          s"[ $code ]"
        } else {
          code
        }
      }

      def declaration = Util.maybeWrapInParens(
        functions.foldLeft(currentVar) { case (v, f) =>
          Util.maybeWrapInParens(f, v)
        }
      )

      p.typ match {
        case ElmString => {
          args.imports.addExposing("Url.Builder", "string")

          s"string \"${p.p.name}\" $declaration"
        }
        case ElmBool => asString("boolToString")
        case ElmInt => asString("String.fromInt")
        case ElmFloat => asString("String.fromFlow")
        case ElmEnumLocal(name) => asString(Names.camelCase(name) + "ToString")
        case ElmEnumImported(ns, name) => {
          args.imports.addExposing(ns, name)
          asString(Names.camelCase(name) + "ToString")
        }
        case ElmMaybe(inner) => {
          val code = inner match {
            case ElmList(_) => s"${innerType(inner)}"
            case _ => s"[${innerType(inner)}]"
          }
          s"(Maybe.withDefault [] (Maybe.map (\\$nextVar -> $code) $declaration))"
        }
        case ElmList(inner) => s"List.map (\\$nextVar -> ${innerType(inner)}) $currentVar"
        case ElmUserDefinedLocal(inner) => asString(Names.camelCase(inner) + "ToString")
        case ElmUserDefinedImported(ns, inner) => {
          args.imports.addExposing(ns, inner)
          asString(Names.camelCase(inner) + "ToString")
        }
        case ElmDate | ElmDict(_) | ElmPosix | ElmNothing => sys.error(s"Do not know how to convert parameter named ${p.name} with type ${p.typ} to a query parameter")
      }
    }

    def generate(): ValidatedNec[String, String] = {

      (validateParameters(),
        validateBody(op.body)
      ).mapN { case (params, bodyType) =>
        generateMethod(op.method, params, bodyType)
      }
    }

    private[this] case class ValidatedParameter(p: Parameter, typ: ElmType) {
      val name: String = p.name
    }

    private[this] def validateParameters(): ValidatedNec[String, Seq[ValidatedParameter]] = {
      op.parameters.map { p =>
        elmType.validate(p.`type`, required = p.required).map { t => ValidatedParameter(p, t) }
      }.sequence.map(removeCommunityId)
    }

    private[this] def removeCommunityId(all: Seq[ValidatedParameter]): Seq[ValidatedParameter] = {
      all.filterNot { p =>
        p.name == "community_id" && p.typ == ElmType.ElmString
      }
    }

    private[this] def makePropsTypeAlias(params: Seq[ValidatedParameter]): Option[ElmVariable] = {
      params.foldLeft(ElmTypeAliasBuilder("props", propsType)) { case (builder, p) =>
        builder.addProperty(p.name, p.typ)
      }.build()
    }

    private[this] def validateBody(body: Option[Body]): ValidatedNec[String, Option[ElmType]] = {
      body match {
        case None => None.validNec
        case Some(b) => elmType.validate(b.`type`, required = true).map(Some(_))
      }
    }

    private[this] def generateMethod(method: Method, params: Seq[ValidatedParameter], body: Option[ElmType]): String = {
      val param = makePropsTypeAlias(params)
      val variable = param.getOrElse {
        ElmParameter("placeholder", "String")
      }

      val function = param.toSeq.foldLeft(ElmFunctionBuilder(name)) { case (builder, _) =>
          builder.addParameter(variable.name, variable.typeName)
        }
        .addParameter("body", body.map(_.declaration))
        .addParameter("params", "HttpRequestParams msg")
        .addReturnType("Cmd msg")
        .addBody(
          // TODO: Move Env.config.apiHost to config
          s"""
             |Http.request
             |    { method = "${method.toString.toUpperCase}"
             |    , url = Env.config.apiHost ++ ${url(variable, params)}
             |    , expect = params.expect
             |    , headers = params.headers
             |    , timeout = Nothing
             |    , tracker = Nothing
             |    , body = ${body.map(b => s"Http.jsonBody (${ElmJson.encoderName(b)} body)").getOrElse("Http.emptyBody")}
             |    }
             |""".stripMargin).build()

      Seq(
        param.flatMap {
          case _: ElmParameter => None
          case a: ElmTypeAlias => Some(a)
        },
        Some(function)
      ).flatten.map(_.code).mkString("\n\n")
    }
  }
}
