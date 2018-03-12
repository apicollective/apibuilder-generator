package scala.generator.mock

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text._
import models.http4s.mock.Http4s018MockClientGenerator

import scala.models.ApidocComments
import scala.generator._

object MockClientGenerator {

  object Play24 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play24(ssd.namespaces.base, None)).invoke()
    }

  }

  object Play25 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play25(ssd.namespaces.base, None)).invoke()
    }

  }

  object Play26 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play26(ssd.namespaces.base, None)).invoke()
    }

  }

  object Ning19 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Ning19(ssd.namespaces.base, None)).invoke()
    }

  }

  object Http4s017 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Http4s017(ssd.namespaces.base, None)).invoke()
    }

  }

  object Http4s018 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service)
      new Http4s018MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Http4s018(ssd.namespaces.base, None)).invoke()
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
    val header = ApidocComments(ssd.service.version, userAgent).toJavaString() + "\n"
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

  def clientCode =
    Seq(
      s"trait Client extends ${ssd.namespaces.interfaces}.Client {",
      s"""  ${config.formatBaseUrl(Some("http://mock.localhost"))}""",
      ssd.resources.map { resource =>
        s"override def ${generator.methodName(resource)}: ${ssd.namespaces.base}.${resource.plural} = Mock${resource.plural}Impl"
      }.mkString("\n").indent(2),
      "}",
      ssd.resources.map { resource =>
        generateMockResource(resource)
      }.mkString("\n\n")
    ).mkString("\n\n")

  def factoriesCode = Seq(
    "object Factories {",
    Seq(
      "def randomString(): String = {",
      """  "Test " + _root_.java.util.UUID.randomUUID.toString.replaceAll("-", " ")""",
      "}"
    ).mkString("\n").indent(2),
    Seq(
      ssd.enums.map { makeEnum },
      ssd.models.map { makeModel },
      ssd.unions.map { makeUnion }
    ).flatten.mkString("\n\n").indent(2),
    "}"
  ).mkString("\n\n")

  def generateCode(): String = {
    Seq(
      s"package ${ssd.namespaces.mock} {",
      Seq(
        ssd.resources match {
          case Nil => None
          case _ => Some(clientCode)
        },
        Some(factoriesCode)
      ).flatten.mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  def makeEnum(enum: ScalaEnum): String = {
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

  def makeModel(model: ScalaModel): String = {
    Seq(
      s"def make${model.name}(): ${model.qualifiedName} = ${model.qualifiedName}(",
      model.fields.map { field =>
        s"${field.name} = ${mockValue(field.datatype)}"
      }.mkString(",\n").indent(2),
      ")"
    ).mkString("\n")
  }

  def makeUnion(union: ScalaUnion): String = {
    val typ = union.types.headOption.getOrElse {
      sys.error("Union type[${union.qualifiedName}] does not have any times")
    }
    s"def make${union.name}(): ${union.qualifiedName} = ${mockValue(typ.datatype)}"
  }

  def generateMockResource(resource: ScalaResource): String = {
    Seq(
      s"object Mock${resource.plural}Impl extends Mock${resource.plural}",
      s"trait Mock${resource.plural} extends ${ssd.namespaces.base}.${resource.plural} {",
      generator.methods(resource).map { m =>
        Seq(
          m.interface + s" = ${config.wrappedAsyncType("Sync").getOrElse(config.asyncType)}.${config.asyncSuccess} {",
          mockImplementation(m).indent(2),
          "}"
        ).mkString("\n")
      }.mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

    def mockImplementation(cm: ScalaClientMethod): String = {
    cm.operation.responses.find(_.isSuccess) match {
      case None => {
        "// No-op as there is no successful response defined"
      }
      case Some(r) => {
        mockValue(ssd.scalaDatatype(r.`type`))
      }
    }
  }

  def mockValue(datatype: ScalaDatatype): String = {
    datatype match {
      case ScalaPrimitive.Boolean => "true"
      case ScalaPrimitive.Double => "1.0"
      case ScalaPrimitive.Integer => "1"
      case ScalaPrimitive.Long => "1l"
      case ScalaPrimitive.DateIso8601Joda => "new org.joda.time.LocalDate()"
      case ScalaPrimitive.DateIso8601Java => "java.time.LocalDate.now"
      case ScalaPrimitive.DateTimeIso8601Joda => "org.joda.time.DateTime.now"
      case ScalaPrimitive.DateTimeIso8601Java => "java.time.Instant.now"
      case ScalaPrimitive.Decimal => """BigDecimal("1")"""
      case ScalaPrimitive.ObjectAsPlay => "play.api.libs.json.Json.obj()"
      case ScalaPrimitive.ObjectAsCirce => "Map()"
      case ScalaPrimitive.JsonValueAsPlay => "play.api.libs.json.Json.obj().asInstanceOf[play.api.libs.json.JsValue]"
      case ScalaPrimitive.JsonValueAsCirce => "io.circe.Json.obj()"
      case ScalaPrimitive.String => "Factories.randomString()"
      case ScalaPrimitive.Unit => "// unit type"
      case ScalaPrimitive.Uuid => "java.util.UUID.randomUUID"
      case ScalaDatatype.List(_) => "Nil"
      case ScalaDatatype.Map(_) => "Map()"
      case ScalaDatatype.Option(_) => "None"
      case ScalaPrimitive.Enum(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.Model(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.Union(ns, name) => s"${ns.mock}.Factories.make$name()"
    }
  }

}
