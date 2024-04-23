package generator.elm

import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.{Method, Operation, Parameter, Resource}

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

      def wrap(fun: String): String = Util.wrapInParens(fun, code)

      params.find(_.name == variable) match {
        case None => code
        case Some(p) => p.typ match {
          case ElmString => code
          case ElmInt => wrap("String.fromInt")
          case ElmFloat => wrap("String.fromFloat")
          case _ => sys.error(s"Do not know how to convert parameter named ${p.name} with type ${p.typ} to String")
        }
      }

    }

    private[this] def url(params: Seq[ValidatedParameter]): String = {
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
              case ":community_id" => {
                "Env.config.community.id"
              }
              case _ => {
                println(s"word: $w")
                val code = s"props.${Names.camelCase(w)}"
                if (w.startsWith(":")) {
                  handlePossibleToString(params, w.drop(1), code)
                } else {
                  s"props.${Names.camelCase(w)}"
                }
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
      buildUrl(op.path, Nil).mkString(" ++ ")
    }

    def generate(): ValidatedNec[String, String] = {
      import io.apibuilder.spec.v0.models.Method._

      validateParameters().map { params =>
        generateMethod(op.method, params)
      }
    }

    private[this] case class ValidatedParameter(p: Parameter, typ: ElmType) {
      val name: String = p.name
    }

    private[this] def validateParameters(): ValidatedNec[String, Seq[ValidatedParameter]] = {
      op.parameters.map { p =>
        elmType.validate(p.`type`).map { t => ValidatedParameter(p, t) }
      }.sequence.map(removeCommunityId)
    }

    private[this] def removeCommunityId(all: Seq[ValidatedParameter]): Seq[ValidatedParameter] = {
      all.filterNot { p =>
        p.name == "community_id" && p.typ == ElmType.ElmString
      }
    }

    private[this] def makePropsTypeAlias(params: Seq[ValidatedParameter]) = {
      params.foldLeft(ElmTypeAliasBuilder(propsType)) { case (builder, p) =>
        builder.addProperty(p.name, p.typ)
      }.build()
    }

    private[this] def generateMethod(method: Method, params: Seq[ValidatedParameter]): String = {
      val propsTypeAlias = makePropsTypeAlias(params)
      val function = propsTypeAlias.toSeq.foldLeft(ElmFunctionBuilder(name)) { case (builder, _) =>
          builder.addParameter("props", propsType)
        }
        .addParameter("params", "HttpRequestParams msg")
        .addReturnType("Cmd msg")
        .addBody(
          s"""
             |Http.request
             |    { method = "${method.toString.toUpperCase}"
             |    , url = Env.config.apiHost ++ ${url(params)}
             |    , expect = params.expect
             |    , headers = params.headers
             |    , timeout = Nothing
             |    , tracker = Nothing
             |    , body = Http.emptyBody
             |    }
             |""".stripMargin).build()

      Seq(
        propsTypeAlias,
        Some(function)
      ).flatten.map(_.code).mkString("\n\n")
    }
  }
}
