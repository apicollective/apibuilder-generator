package scala.generator.flow

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import scala.models.ApidocComments
import scala.generator._

object FlowMockClientGenerator {

  object Play26 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new FlowMockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play26(ssd.namespaces.base, None)).invoke()
    }

  }

}

class FlowMockClientGenerator(
  ssd: ScalaService,
  userAgent: Option[String],
  config: ScalaClientMethodConfig
) {
  private[this] val generator = new ScalaClientMethodGenerator(config, ssd)

  def invoke(): Either[Seq[String], Seq[File]] = {
    val header = ApidocComments(ssd.service.version, userAgent).toJavaString() + "\n"
    val code = generateCode()

    Right(
      Seq(
        ServiceFileNames.toFile(
          ssd.service.namespace,
          ssd.service.organization.key,
          ssd.service.application.key,
          ssd.service.version,
          s"Mock${ssd.service.name}Client",
          header ++ code,
          Some("Scala")
        )
      )
    )
  }

  def generateCode(): String = {
s"""
package ${ssd.namespaces.base}

import io.flow.common.v0.models.UserReference
import io.flow.play.util.{AuthHeaders, FlowSession}
import ${ssd.namespaces.base}.Client
import io.flow.test.utils.FlowMockClient

trait Mock${ssd.name}Client extends FlowMockClient[
   ${ssd.namespaces.base}.Client,
   ${ssd.namespaces.base}.errors.GenericErrorResponse,
   ${ssd.namespaces.base}.errors.UnitResponse
 ]{

 override def createAnonymousClient(baseUrl: String): ${ssd.namespaces.base}.Client =
   new ${ssd.namespaces.base}.Client(
     ws = wsClient,
     baseUrl = baseUrl
   )

 override def createIdentifiedClient(baseUrl: String, user: UserReference, org: Option[String], session: Option[FlowSession]): Client = {
   val auth = org match {
     case None =>  AuthHeaders.user(user, session = session)
     case Some(o) => AuthHeaders.organization(user, o, session = session)
   }

   new ${ssd.namespaces.base}.Client(
     ws = wsClient,
     baseUrl = baseUrl,
     defaultHeaders = authHeaders.headers(auth)
   )
 }
}
""".stripMargin


  }

}
