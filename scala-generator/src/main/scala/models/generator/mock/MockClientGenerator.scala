package scala.generator.mock

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{ResponseCodeInt, ResponseCodeOption, ResponseCodeUndefinedType}
import lib.Text._
import lib.generator.CodeGenerator

import scala.generator._
import scala.models.{ApiBuilderComments, Attributes}

object MockClientGenerator {

  object Play24 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play24(ssd.namespaces.base, Attributes.PlayDefaultConfig.withAttributes(form.attributes), None)).invoke()
    }

  }

  object Play25 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play25(ssd.namespaces.base, Attributes.PlayDefaultConfig.withAttributes(form.attributes), None)).invoke()
    }

  }

  object Play26 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play26(ssd.namespaces.base, Attributes.PlayDefaultConfig.withAttributes(form.attributes), None)).invoke()
    }

  }

  object Play28 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play28(ssd.namespaces.base, Attributes.PlayDefaultConfig.withAttributes(form.attributes), None)).invoke()
    }

  }

  object Ning19 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Ning19(ssd.namespaces.base, Attributes.PlayDefaultConfig, None)).invoke()
    }
  }
}

class MockClientGenerator(
  ssd: ScalaService,
  userAgent: Option[String],
  config: ScalaClientMethodConfig
) {
  val generator = new ScalaClientMethodGenerator(config, ssd)

  def invoke(): Either[Seq[String], Seq[File]] = {
    val header = ApiBuilderComments(ssd.service.version, userAgent).toJavaString + "\n"
    val code = generateCode()

    Right(
      Seq(
        ServiceFileNames.toFile(
          ssd.service.namespace,
          ssd.service.organization.key,
          ssd.service.application.key,
          ssd.service.version,
          "MockClient",
          header ++ code,
          Some("Scala")
        )
      )
    )
  }

  def clientCode: String =
    Seq(
      s"trait Client extends ${ssd.namespaces.codeGenInterfaces}.Client {",
      s"""  ${config.formatBaseUrl(Some("http://mock.localhost"))}""",
      ssd.resources.map { resource =>
        s"override def ${generator.methodName(resource)}: ${ssd.namespaces.base}.${resource.plural} = Mock${resource.plural}Impl"
      }.mkString("\n").indentString(2),
      "}",
      ssd.resources.map { resource =>
        generateMockResource(resource)
      }.mkString("\n\n")
    ).mkString("\n\n")

  def generateCode(): String = {
    Seq(
      s"package ${ssd.namespaces.mock} {",
      Seq(
        ssd.resources match {
          case Nil => None
          case _ => Some(clientCode)
        },
        Some(MockFactoriesGenerator.factoriesCode(ssd))
      ).flatten.mkString("\n\n").indentString(2),
      "}"
    ).mkString("\n\n")
  }

  def makeEnum(`enum`: ScalaEnum): String = {
    val name = enum.values.headOption match {
      case None => {
        """UNDEFINED("other")"""
      }
      case Some(value) => {
        value.name
      }
    }
    s"def make${enum.name}(): ${enum.qualifiedName} = ${enum.qualifiedName}.$name"
  }


  def generateMockResource(resource: ScalaResource): String = {
    Seq(
      s"object Mock${resource.plural}Impl extends Mock${resource.plural}",
      s"trait Mock${resource.plural} extends ${ssd.namespaces.base}.${resource.plural} {",
      generator.methods(resource).map { m =>
        Seq(
          m.interface + s" = ${config.asyncSuccessInvoke} {",
          mockImplementation(m).indentString(2),
          "}"
        ).mkString("\n")
      }.mkString("\n\n").indentString(2),
      "}"
    ).mkString("\n\n")
  }

  def mockImplementation(cm: ScalaClientMethod): String = {
    cm.operation.responses.find(_.isSuccess) match {
      case None => {
        "// No-op as there is no successful response defined"
      }
      case Some(r) => {
        val unitType = config.responseEnvelopeClassName.map { _ => "()" }.getOrElse("// unit type")
        val resultType = MockFactoriesGenerator.mockValue(ssd.scalaDatatype(r.`type`), unitType = unitType)
        config.responseEnvelopeClassName match {
          case None => resultType
          case Some(envelopeName) => {
            Seq(
              s"${ssd.namespaces.base}.${envelopeName}Impl(",
              Seq(
                s"body = $resultType,",
                s"status = ${getStatus(r)},",
                s"headers = ${ssd.namespaces.base}.ResponseHeaders(Map.empty)",
              ).mkString("\n").indentString(2),
              ")"
            ).mkString("\n")
          }
        }
      }
    }
  }

  private def getStatus(r: ScalaResponse): Int = {
    r.code match {
      case ResponseCodeInt(value) => value
      case ResponseCodeOption.Default => 200
      case ResponseCodeOption.UNDEFINED(_) => 417
      case ResponseCodeUndefinedType(_) => 500
    }
  }
}
