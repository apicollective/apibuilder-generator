package generator.elm

import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.{Operation, Parameter, Resource}

import scala.annotation.tailrec

case class ElmResource(args: GenArgs) {
  //private[this] val elmJson = ElmJson(args.imports)
  private[this] val elmType = ElmType(args)


  def generate(resource: Resource): ValidatedNec[String, String] = {
    elmType.validate(resource.`type`).andThen { resourceType =>
      resource.operations.map { op =>
        Generator(resource, resourceType, op).generate()
      }.sequence.map(_.mkString("\n\n"))
    }
  }

  case class Generator(resource: Resource, resourceType: String, op: Operation) {
    private[this] val name: String = {
      val params = op.path.drop(resource.path.map(_.length).getOrElse(0)).split("/").filter(_.startsWith(":")).map(Names.pascalCase)
      val prefix = op.method.toString.toLowerCase() + Names.pascalCase(resourceType)
      if (params.isEmpty) {
        prefix
      } else {
        prefix + "By" + params.mkString("And")
      }
    }
    private[this] val url: String = {
      @tailrec
      def buildUrl(remaining: String, u: Seq[String]): Seq[String] = {
        val i = remaining.indexOf(":")
        if (i < 0) {
          u ++ Seq(Util.wrapInQuotes(remaining))
        } else {
          val prefix = remaining.take(i)
          val endIndex = remaining.drop(i).indexOf("/")

          def gen(w: String) = u ++ Seq(Util.wrapInQuotes(prefix) + s" ++ props.${Names.camelCase(w)}")
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
        op.method match {
          case Delete => delete(params)
          case _ => ""
        }
      }
    }

    private[this] case class ValidatedParameter(p: Parameter, typ: String) {
      val name: String = p.name
    }

    private[this] def validateParameters(): ValidatedNec[String, Seq[ValidatedParameter]] = {
      op.parameters.map { p =>
        elmType.validate(p.`type`).map { t => ValidatedParameter(p, t) }
      }.sequence
    }

    private[this] def delete(params: Seq[ValidatedParameter]): String = {
      val propsType = Names.pascalCase(name) + "Props"
      val propsTypeAlias: Option[ElmCode] = params.foldLeft(ElmTypeAliasBuilder(propsType)) { case (builder, p) =>
        builder.addProperty(p.name, p.typ)
      }.build()

      val function = propsTypeAlias.toSeq.foldLeft(ElmFunctionBuilder(name)) { case (builder, p) =>
          builder.addParameter("props", propsType)
        }
        .addParameter("params", "HttpRequestParams msg")
        .addReturnType("Cmd msg")
        .addBody(
          s"""
             |Http.request
             |    { method = "DELETE"
             |    , url = $url
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
