package scala.models.http4s

import io.apibuilder.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}

import scala.generator._

class ScalaClientMethodGeneratorV0_23 (
  config: ScalaClientMethodConfig,
  ssd: ScalaService
)  extends scala.models.http4s.ScalaClientMethodGenerator(config, ssd) {
  import lib.Text._

  override def objects(): String = {
    sortedResources.map { resource =>
      Seq(
        Some(ScalaUtil.deprecationString(resource.deprecation).trim).filter(_.nonEmpty),
        Some(
          s"object ${resource.plural} extends ${resource.plural}${config.wrappedAsyncType().getOrElse("")} {\nimport Client._\n" +
            methods(resource).map(_.code).mkString("\n\n").indentString(2) +
            "\n}"
        )
      ).flatten.mkString("\n")
    }.mkString("\n\n")
  }


  override def methods(resource: ScalaResource): Seq[scala.models.http4s.ScalaClientMethod] = {
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
        args.append("body = payload, formBody = formPayload")
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

      val hasOptionResult = featureMigration.hasImplicit404s() match {
        case true => {
          op.responses.filter(_.isSuccess).find(_.isOption).map { _ =>
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
            s"case r => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod}))"
          } else {
            config match {
              case _: ScalaClientMethodConfigs.Http4s017 | _: ScalaClientMethodConfigs.Http4s015 =>
                s"case r => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.${response.errorClassName}(r))"
              case _ =>
                val json = config.toJson("r", response.datatype.name)
                s"case r => $json.flatMap(body => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.${response.errorClassName}(r.headers, r.status.code, None, body)))"
            }
          }
        }
        case None => {
          s"""case r => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.FailedRequest(r.${config.responseStatusMethod}, s"Unsupported response code[""" + "${r." + config.responseStatusMethod + s"""}]. Expected: ${allResponseCodes.mkString(", ")}"${Http4sScalaClientCommon.failedRequestUriParam(config)}))"""
        }
      }

      val matchResponse: String = {
        op.responses.flatMap { response =>
          response.code match {
            case ResponseCodeInt(statusCode) => {
              if (response.isSuccess) {
                if (featureMigration.hasImplicit404s() && response.isOption) {
                  if (response.isUnit) {
                    Some(s"case r if r.${config.responseStatusMethod} == $statusCode => ${http4sConfig.asyncSuccessInvoke}(Some(()))")
                  } else {
                    val json = config.toJson("r", response.datatype.name)
                    Some(s"case r if r.${config.responseStatusMethod} == $statusCode => ${http4sConfig.asyncSuccessInvoke}(Some($json))")
                  }

                } else if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => ${http4sConfig.asyncSuccessInvoke}(())")

                } else {
                  val json = config.toJson("r", response.datatype.name)
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $json")
                }
              } else if (featureMigration.hasImplicit404s() && response.isNotFound && response.isOption) {
                // will be added later
                None
              } else {
                if (response.isUnit) {
                  Some(s"case r if r.${config.responseStatusMethod} == $statusCode => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.${response.errorClassName}(r.${config.responseStatusMethod}))")
                } else {
                  config match {
                    case _: ScalaClientMethodConfigs.Http4s017 | _: ScalaClientMethodConfigs.Http4s015 =>
                      Some(s"case r if r.${config.responseStatusMethod} == $statusCode => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.${response.errorClassName}(r))")
                    case _ =>
                      val json = config.toJson("r", response.datatype.name)
                      Some(s"case r if r.${config.responseStatusMethod} == $statusCode => $json.flatMap(body => ${http4sConfig.wrappedAsyncType("Concurrent").getOrElse(http4sConfig.asyncType)}.${http4sConfig.asyncFailure}(new ${namespaces.errors}.${response.errorClassName}(r.headers, r.status.code, None, body)))")
                  }
                }
              }
            }

            case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => {
              None
            }
          }
        }.mkString("\n")
      } + hasOptionResult.getOrElse("") + s"\n$defaultResponse\n"

      new scala.models.http4s.ScalaClientMethod(
        operation = op,
        returnType = s"${config.asyncType}[$resType]",
        methodCall = methodCall,
        response = matchResponse,
        implicitArgs = config.implicitArgs,
      )
    }
  }
}
