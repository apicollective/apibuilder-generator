package generator

import models.JsonImports
import com.gilt.apidoc.spec.v0.models.{Resource, ResponseCode, ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import scala.collection.immutable.TreeMap

case class ScalaClientMethodGenerator(
  config: ScalaClientMethodConfig,
  ssd: ScalaService
) {
  import lib.Text
  import lib.Text._

  private val namespaces = Namespaces(config.namespace)

  private val generatorUtil = GeneratorUtil(config)

  private val sortedResources = ssd.resources.sortWith { _.plural.toLowerCase < _.plural.toLowerCase }

  def traitsAndErrors(): String = {
    (traits() + "\n\n" + errorPackage()).trim
  }

  def accessors(): String = {
    sortedResources.map { resource =>
      val methodName = lib.Text.snakeToCamelCase(lib.Text.camelCaseToUnderscore(resource.plural).toLowerCase)
      config.accessor(methodName, resource.plural)
    }.mkString("\n\n")
  }

  def traits(): String = {
    sortedResources.map { resource =>
      s"trait ${resource.plural} {\n" +
      methods(resource).map(_.interface).mkString("\n\n").indent(2) +
      "\n}"
    }.mkString("\n\n")
  }

  def objects(): String = {
    sortedResources.map { resource =>
      s"object ${resource.plural} extends ${resource.plural} {\n" +
      methods(resource).map(_.code).mkString("\n\n").indent(2) +
      "\n}"
    }.mkString("\n\n")
  }

  def errorPackage(): String = {
    Seq(
      Some("package errors {"),
      modelErrorClasses() match {
        case Nil => None
        case classes => {
          Some(JsonImports(ssd.service).mkString("\n").indent(2) + "\n\n" + classes.mkString("\n\n").indent(2))
        }
      },
      Some(failedRequestClass().indent(2)),
      Some("}")
    ).flatten.mkString("\n\n")
  }

  private def failedRequestClass(): String = {
    """case class FailedRequest(responseCode: Int, message: String, requestUri: Option[_root_.java.net.URI] = None) extends Exception(s"HTTP $responseCode: $message")"""
  }

  /**
    * Returns custom case classes based on the service description for
    * all error return types. e.g. a 409 that returns Seq[Error] is
    * handled via these classes.
    */
  private def modelErrorClasses(): Seq[String] = {
    ssd.resources.flatMap(_.operations).flatMap(_.responses).filter(r => !(r.isSuccess || r.isUnit)).map { response =>
      errorTypeClass(response)
    }.distinct.sorted
  }

  private[this] def errorTypeClass(response: ScalaResponse): String = {
    require(!response.isSuccess)

    val json = config.toJson("response", response.datatype.name)
    exceptionClass(
      response.errorClassName,
      Some(s"lazy val ${response.errorVariableName} = ${json.indent(2).trim}")
    )
  }

  private[this] def exceptionClass(
    className: String,
    body: Option[String] = None
  ): String = {
    val bodyString = body match {
      case None => ""
      case Some(b) => "{\n" + b.indent(2) + "\n}"
    }

    Seq(
      s"case class $className(",
      s"  response: ${config.responseClass},",
      s"  message: Option[String] = None",
      s""") extends Exception(message.getOrElse(response.${config.responseStatusMethod} + ": " + response.${config.responseBodyMethod}))$bodyString"""
    ).mkString("\n")

  }

  private[this] def methods(resource: ScalaResource): Seq[ClientMethod] = {
    resource.operations.map { op =>
      val path = generatorUtil.pathParams(op)

      val payload = generatorUtil.formBody(op)
      val queryParameters = generatorUtil.queryParameters("queryParameters", op.queryParameters)

      val code = new scala.collection.mutable.ListBuffer[String]()
      val args = new scala.collection.mutable.ListBuffer[String]()
      payload.foreach { v =>
        code.append(v)
        args.append("body = Some(payload)")
      }

      queryParameters.foreach { v =>
        code.append(v)
        args.append("queryParameters = queryParameters")
      }

      val methodCall = code.toList match {
        case Nil => s"""_executeRequest("${op.method}", $path)"""
        case v => s"""${v.mkString("\n\n")}\n\n_executeRequest("${op.method}", $path, ${args.mkString(", ")})"""
      }

      val hasOptionResult = op.responses.filter(_.isSuccess).find(_.isOption).map { r =>
        s"\ncase r if r.${config.responseStatusMethod} == 404 => None"
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
          s"case r => throw new ${namespaces.errors}.${response.errorClassName}(r)"
        }
        case None => {
          s"""case r => throw new ${namespaces.errors}.FailedRequest(r.${config.responseStatusMethod}, s"Unsupported response code[""" + "${r." + config.responseStatusMethod + s"""}]. Expected: ${allResponseCodes.mkString(", ")}"${ScalaClientObject.failedRequestUriParam(config)})"""
        }
      }

      val matchResponse: String = {
        op.responses.flatMap { response =>
          response.code match {
            case ResponseCodeInt(statusCode) => {
              if (response.isSuccess) {
                if (response.isOption) {
                  if (response.isUnit) {
                    Some(s"case r if r.${config.responseStatusMethod} == ${statusCode} => Some(Unit)")
                  } else {
                    val json = config.toJson("r", response.datatype.name)
                    Some(s"case r if r.${config.responseStatusMethod} == ${statusCode} => Some($json)")
                  }

                } else if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == ${statusCode} => ${response.datatype.name}")

                } else {
                  val json = config.toJson("r", response.datatype.name)
                  Some(s"case r if r.${config.responseStatusMethod} == ${statusCode} => $json")
                }

              } else if (response.isNotFound && response.isOption) {
                // will be added later
                None

              } else {
                Some(s"case r if r.${config.responseStatusMethod} == ${statusCode} => throw new ${namespaces.errors}.${response.errorClassName}(r)")
              }
            }

            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
              None
            }
          }
        }.mkString("\n")
      } + hasOptionResult.getOrElse("") + s"\n$defaultResponse\n"

      ClientMethod(
        name = op.name,
        argList = op.argList,
        returnType = hasOptionResult match {
          case None => s"scala.concurrent.Future[${op.resultType}]"
          case Some(_) => s"scala.concurrent.Future[_root_.scala.Option[${op.resultType}]]"
        },
        methodCall = methodCall,
        response = matchResponse,
        comments = op.description
      )

    }
  }


  case class ClientMethod(
    name: String,
    argList: Option[String],
    returnType: String,
    methodCall: String,
    response: String,
    comments: Option[String]
  ) {
    import lib.Text._
    
    private val commentString = comments.map(string => ScalaUtil.textToComment(string) + "\n").getOrElse("")

    val interface: String = {
      s"""${commentString}def $name(${argList.getOrElse("")})(implicit ec: scala.concurrent.ExecutionContext): $returnType"""
    }

    val code: String = {
      s"""override def $name(${argList.getOrElse("")})(implicit ec: scala.concurrent.ExecutionContext): $returnType = {
${methodCall.indent}.map {
${response.indent(4)}
  }
}"""
    }
  }

}
