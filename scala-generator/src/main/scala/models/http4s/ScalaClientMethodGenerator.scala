package scala.models.http4s

import com.bryzek.apidoc.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import scala.generator.{ScalaClientMethodConfig, ScalaUtil, ScalaResource, ScalaOperation}

class ScalaClientMethodGenerator (
  config: ScalaClientMethodConfig,
  ssd: ScalaService
)  extends scala.generator.ScalaClientMethodGenerator(config, ssd) {
  import lib.Text._

  override protected val generatorUtil = new ScalaGeneratorUtil(config)

  override def interfaces(): String = {
    Seq(
      "package interfaces {",
      Seq(
        "trait Client {",
        "  def baseUrl: org.http4s.Uri",
        sortedResources.map { resource =>
          s"def ${methodName(resource)}: ${namespaces.base}.${resource.plural}"
        }.mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  override protected def failedRequestClass(): String = {
    """case class FailedRequest(responseCode: Int, message: String, requestUri: Option[_root_.java.net.URI] = None, parent: Exception = null) extends _root_.java.lang.Exception(s"HTTP $responseCode: $message", parent)"""
  }

  override def methods(resource: ScalaResource): Seq[ScalaClientMethod] = {
    resource.operations.map { op =>
      val payload = generatorUtil.formBody(op, canSerializeUuid = config.canSerializeUuid)
      val queryParameters = generatorUtil.queryParameters("queryParameters", op.queryParameters)
      val headerParameters = generatorUtil.queryParameters("headerParameters", op.headerParameters)

      val code = new scala.collection.mutable.ListBuffer[String]()
      val args = new scala.collection.mutable.ListBuffer[String]()

      code.append(s"val urlPath = ${generatorUtil.pathParams(op)}")
      args.append("path = urlPath")

      payload.foreach { v =>
        code.append(v)
        args.append("body = Some(payload)")
      }

      queryParameters.foreach { v =>
        code.append(v)
        args.append("queryParameters = queryParameters")
      }
      headerParameters.fold(
        args.append("requestHeaders = requestHeaders")
      ){ h =>
        code.append(h)
        args.append("requestHeaders = (requestHeaders ++ headerParameters)")
      }

      val reqType = op.body.fold("Unit")(b => b.datatype.name)

      val hasOptionResult = featureMigration.hasImplicit404s match {
        case true => {
          op.responses.filter(_.isSuccess).find(_.isOption).map { r =>
            s"\ncase r if r.${config.responseStatusMethod} == 404 => None"
          }
        }
        case false => None
      }

      val resType: String = hasOptionResult match {
        case None => op.resultType
        case Some(_) => s"_root_.scala.Option[${op.resultType}]"
      }

      val methodCall = code.toList match {
        case Nil => s"""_executeRequest[$reqType, $resType]("${op.method}", ${args.mkString(", ")})"""
        case v => s"""${v.mkString("\n\n")}\n\n_executeRequest[$reqType, $resType]("${op.method}", ${args.mkString(", ")})"""
      }

      val allResponseCodes = (
        op.responses.flatMap { r =>
          r.code match {
            case ResponseCodeInt(value) => Some(value)
            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => None
          }
        } ++ (hasOptionResult match {
          case None => Seq.empty
          case Some(_) => Seq(404)
        })
      ).distinct.sorted

      val defaultResponse = op.responses.find { r =>
        r.code match {
          case ResponseCodeOption.Default => true
          case ResponseCodeOption.UNDEFINED(_) | ResponseCodeInt(_) | ResponseCodeUndefinedType(_) => false
        }
      } match {
        case Some(response) => {
          if (response.isUnit) {
            s"case r => Task.fail(new ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod}))"
          } else {
            s"case r => Task.fail(new ${namespaces.errors}.${response.errorClassName}(r))"
          }
        }
        case None => {
          s"""case r => Task.fail(new ${namespaces.errors}.FailedRequest(r.${config.responseStatusMethod}, s"Unsupported response code[""" + "${r." + config.responseStatusMethod + s"""}]. Expected: ${allResponseCodes.mkString(", ")}"${Http4sScalaClientCommon.failedRequestUriParam(config)}))"""
        }
      }

      val matchResponse: String = {
        op.responses.flatMap { response =>
          response.code match {
            case ResponseCodeInt(statusCode) => {
              if (response.isSuccess) {
                if (featureMigration.hasImplicit404s && response.isOption) {
                  if (response.isUnit) {
                    Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Task.now(Some(()))")
                  } else {
                    val json = config.toJson("r", response.datatype.name)
                    Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Task.now(Some($json))")
                  }

                } else if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Task.now(())")

                } else {
                  val json = config.toJson("r", response.datatype.name)
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $json")
                }

              } else if (featureMigration.hasImplicit404s && response.isNotFound && response.isOption) {
                // will be added later
                None

              } else {
                if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Task.fail(new ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod}))")

                } else {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => Task.fail(new ${namespaces.errors}.${response.errorClassName}(r))")
                }
              }
            }

            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
              None
            }
          }
        }.mkString("\n")
      } + hasOptionResult.getOrElse("") + s"\n$defaultResponse\n"

      new ScalaClientMethod(
        operation = op,
        returnType = s"${config.asyncType}[$resType]",
        methodCall = methodCall,
        response = matchResponse,
        implicitArgs = config.implicitArgs
      )

    }
  }
}

class ScalaClientMethod(
  operation: ScalaOperation,
  returnType: String,
  methodCall: String,
  response: String,
  implicitArgs: Option[String]
) extends scala.generator.ScalaClientMethod(operation, returnType, methodCall, response, implicitArgs) {
  import lib.Text._

  override val code: String = {
    s"""${ScalaUtil.deprecationString(operation.deprecation)}override def $name(${argList.getOrElse("")})${implicitArgs.getOrElse("")}: $returnType = {
${methodCall.indent} {
${response.indent(4)}
  }
}"""
  }
}
