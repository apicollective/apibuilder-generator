package scala.generator.mock

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text._

import scala.models.{ApidocComments, Config}
import scala.generator._
import scala.util.Random

object MockClientGenerator {

  object Play24 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play24(ssd.namespaces.base, None)).invoke()
    }

  }

  object Play25 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play25(ssd.namespaces.base, None)).invoke()
    }

  }

  object Play26 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Play26(ssd.namespaces.base, None)).invoke()
    }

  }

  object Ning19 extends CodeGenerator {

    override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
      val ssd = new ScalaService(form.service, Config(form.attributes, Config.PlayDefaultConfig))
      new MockClientGenerator(ssd, form.userAgent, ScalaClientMethodConfigs.Ning19(ssd.namespaces.base, None)).invoke()
    }

  }

  private[mock] def calculateStringLength(limitation: ScalaField.Limitation): Int = {
    val defaultCandidate = 24L

    // TODO handle Int vs Long (really String of length Int.MaxValue + 1??!)
    //   consider some artificial upper bound of e.g. 10,000; 100,000; 1,000,000 -- to be discussed

    val withLimitationApplied = limitation match {
      case ScalaField.Limitation(None, None) => defaultCandidate
      case ScalaField.Limitation(Some(minimum), None) => Math.max(defaultCandidate, minimum)
      case ScalaField.Limitation(None, Some(maximum)) => Math.min(defaultCandidate, maximum)
      case ScalaField.Limitation(Some(minimum), Some(maximum)) =>
        if (minimum < maximum) minimum + Random.nextInt(maximum.toInt - minimum.toInt)
        else if (minimum == maximum) minimum
        else 0
    }

    val withZeroAsLowerBound = Math.max(0L, withLimitationApplied)

    if (!withZeroAsLowerBound.isValidInt) Int.MaxValue // TODO really?!
    else withZeroAsLowerBound.toInt
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
      """def randomString(length: Int = 24): String = {""",
      """  _root_.scala.util.Random.alphanumeric.take(length).mkString""",
      """}"""
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
        s"${field.name} = ${mockValue(field.datatype, Some(field.limitation))}"
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
          m.interface + s" = ${config.asyncSuccessInvoke} {",
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

  def mockValue(datatype: ScalaDatatype, limitation: Option[ScalaField.Limitation] = None): String = {
    datatype match {
      case ScalaPrimitive.Boolean => "true"
      case ScalaPrimitive.Double => "1.0"
      case ScalaPrimitive.Integer => "1"
      case ScalaPrimitive.Long => "1l"
      case dt: ScalaPrimitive.DateIso8601 => s"${dt.fullName}.now"
      case dt: ScalaPrimitive.DateTimeIso8601 => s"${dt.fullName}.now"
      case ScalaPrimitive.Decimal => """BigDecimal("1")"""
      case ScalaPrimitive.ObjectAsPlay => "_root_.play.api.libs.json.Json.obj()"
      case ScalaPrimitive.ObjectAsCirce => "Map()"
      case ScalaPrimitive.JsonValueAsPlay => "_root_.play.api.libs.json.Json.obj().asInstanceOf[_root_.play.api.libs.json.JsValue]"
      case dt @ ScalaPrimitive.JsonValueAsCirce => s"${dt.fullName}.obj()"
      case ScalaPrimitive.String => {
        limitation match {
          case None => "Factories.randomString()"
          case Some(limitationVal) => s"Factories.randomString(${MockClientGenerator.calculateStringLength(limitationVal)})"
        }
      }
      case ScalaPrimitive.Unit => "// unit type"
      case dt @ ScalaPrimitive.Uuid => s"${dt.fullName}.randomUUID"
      case ScalaDatatype.List(_) => "Nil"
      case ScalaDatatype.Map(_) => "Map()"
      case ScalaDatatype.Option(_) => "None"
      case ScalaPrimitive.Enum(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.Model(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.Union(ns, name) => s"${ns.mock}.Factories.make$name()"
    }
  }
}
