package scala.generator.mock

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text._
import scala.models.ApidocComments
import scala.generator._

object MockClientGenerator {

  object Play24 extends CodeGenerator {

    override def invoke(form: InvocationForm) = {
      val ssd = new ScalaService(form.service)
      MockClientGenerator(form, ScalaClientMethodConfigs.Play24(ssd.namespaces.base, None)).invoke()
    }

  }

  object Play25 extends CodeGenerator {

    override def invoke(form: InvocationForm) = {
      val ssd = new ScalaService(form.service)
      MockClientGenerator(form, ScalaClientMethodConfigs.Play25(ssd.namespaces.base, None)).invoke()
    }

  }

}

case class MockClientGenerator(
  form: InvocationForm,
  config: ScalaClientMethodConfig
) {
  private[this] val ssd = new ScalaService(form.service)
  private[this] val generator = ScalaClientMethodGenerator(config, ssd)

  def invoke(): Either[Seq[String], Seq[File]] = {
    val header = ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
    val code = generateCode(ssd)

    Right(
      Seq(
        ServiceFileNames.toFile(
          form.service.namespace,
          form.service.organization.key,
          form.service.application.key,
          form.service.version,
          "MockClient",
          header ++ code,
          Some("Scala")
        )
      )
    )
  }

  private[this] def generateCode(ssd: ScalaService): String = {
    Seq(
      s"package ${ssd.namespaces.mock} {",
      Seq(
        ssd.resources match {
          case Nil => None
          case _ => Some(
            Seq(
              s"trait Client extends ${ssd.namespaces.interfaces}.Client {",
              """  val baseUrl = "http://mock.localhost"""",
              ssd.resources.map { resource =>
                s"override def ${generator.methodName(resource)}: Mock${resource.plural} = Mock${resource.plural}Impl"
              }.mkString("\n").indent(2),
              "}",
              ssd.resources.map { resource =>
                generateMockResource(resource)
              }.mkString("\n\n")
            ).mkString("\n\n")
          )
        },
        Some(
          Seq(
            "object Factories {",
            Seq(
              "def randomString(): String = {",
              """  "Test " + _root_.java.util.UUID.randomUUID.toString.replaceAll("-", " ")""",
              "}"
            ).mkString("\n").indent(2),
            Seq(
              ssd.enums.map { makeEnum(_) },
              ssd.models.map { makeModel(_) },
              ssd.unions.map { makeUnion(_) }
            ).flatten.mkString("\n\n").indent(2),
            "}"
          ).mkString("\n\n")
        )
      ).flatten.mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  private[this] def makeEnum(enum: ScalaEnum): String = {
    val name = enum.values.headOption match {
      case None => {
        """UNDEFINED("other")"""
      }
      case Some(value) => {
        value.name
      }
    }
    s"def make${enum.name}() = ${enum.qualifiedName}.$name"
  }

  private[this] def makeModel(model: ScalaModel): String = {
    Seq(
      s"def make${model.name}() = ${model.qualifiedName}(",
      model.fields.map { field =>
        s"${field.name} = ${mockValue(field.datatype)}"
      }.mkString(",\n").indent(2),
      ")"
    ).mkString("\n")
  }

  private[this] def makeUnion(union: ScalaUnion): String = {
    val typ = union.types.headOption.getOrElse {
      sys.error("Union type[${union.qualifiedName}] does not have any times")
    }
    s"def make${union.name}() = ${mockValue(typ.datatype)}"
  }

  private[this] def generateMockResource(resource: ScalaResource): String = {
    Seq(
      s"object Mock${resource.plural}Impl extends Mock${resource.plural}",
      s"trait Mock${resource.plural} extends ${ssd.namespaces.base}.${resource.plural} {",
      generator.methods(resource).map { m =>
        Seq(
          m.interface + " = scala.concurrent.Future {",
          mockImplementation(m).indent(2),
          "}"
        ).mkString("\n")
      }.mkString("\n\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

    private[this] def mockImplementation(cm: ScalaClientMethod): String = {
    cm.operation.responses.find(_.isSuccess) match {
      case None => {
        "// No-op as there is no successful response defined"
      }
      case Some(r) => {
        mockValue(ssd.scalaDatatype(r.`type`))
      }
    }
  }

  private[this] def mockValue(datatype: ScalaDatatype): String = {
    datatype match {
      case ScalaPrimitive.Boolean => "true"
      case ScalaPrimitive.Double => "1.0"
      case ScalaPrimitive.Integer => "1"
      case ScalaPrimitive.Long => "1l"
      case ScalaPrimitive.DateIso8601 => "new org.joda.time.LocalDate()"
      case ScalaPrimitive.DateTimeIso8601 => "new org.joda.time.DateTime()"
      case ScalaPrimitive.Decimal => """BigDecimal("1")"""
      case ScalaPrimitive.Object => "play.api.libs.json.Json.obj()"
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
